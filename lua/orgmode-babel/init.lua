local M = {
	langs = {},
}

function M.setup(opts)
	opts = opts or opts

	M.langs = opts.langs and opts.langs or {}

	M._here = vim.fn.fnamemodify(debug.getinfo(1).source:sub(2), ":p:h")
	M._run_by_name = M._here .. "/run_by_name.el"
	M._run_by_number = M._here .. "/run_by_number.el"
	M._run_all = M._here .. "/run_all.el"

	M._tangle_by_name = M._here .. "/tangle_by_name.el"
	M._tangle_all = M._here .. "/tangle_all.el"
	M._tangle_by_number = M._here .. "/tangle_by_number.el"

	M._base_cmd = {
		"emacs",
		"--batch",
		"--eval",
		"(require 'org)",
		"--eval",
		"(require 'ob-tangle)",
		"--eval",
		"(setq make-backup-files nil)",
	}

	vim.list_extend(M._base_cmd, {
		"--eval",
		"(org-babel-do-load-languages 'org-babel-load-languages '(" .. vim.fn.reduce(M.langs, function(acc, value)
			return acc .. "(" .. value .. " . t) "
		end, "") .. "))",
	})
end

local named_blocks_query = vim.treesitter.query.parse(
	"org",
	[[
  (body
    (block
      directive: (directive
        name: (expr) (#eq? "name")
        value: (value) @name
      )

      name: (expr) (#eq? "src")
    ) @block
  )
]]
)

local unnamed_blocks_query = vim.treesitter.query.parse(
	"org",
	[[
  (body
    (block
      name: (expr) (#eq? "src")
    ) @block
  )
]]
)

function M.get_names_in_buffer(bufnr, line1, line2)
	bufnr = bufnr or vim.api.nvim_get_current_buf()
	local parser = vim.treesitter.get_parser(bufnr, "org", {})

	local tree = parser:parse()[1]:root()
	local root = tree:root()

	local names = {}

	local range = line1 ~= nil and line2 ~= nil
	local single = range and line1 == line2

	for _, match in named_blocks_query:iter_matches(root, bufnr, 0, -1) do
		local passed = not range
		local name

		for id, node in pairs(match) do
			local row1, col1, row2, col2 = node:range()
			local text = vim.api.nvim_buf_get_text(bufnr, row1, col1, row2, col2, {})[1]

			if named_blocks_query.captures[id] == "block" and not passed then
				if single and line1 - 1 > row1 and line1 - 1 < row2 then
					passed = true
				elseif not single and line1 - 1 <= row1 and row2 <= line2 - 1 then
					passed = true
				end
			end

			if named_blocks_query.captures[id] == "name" then
				name = text
			end
		end

		if passed then
			table.insert(names, name)
		end
	end
	return names
end

function M.get_blocks_in_buffer(bufnr, line1, line2)
	bufnr = bufnr or vim.api.nvim_get_current_buf()
	local parser = vim.treesitter.get_parser(bufnr, "org", {})

	local tree = parser:parse()[1]:root()
	local root = tree:root()

	local indexes = {}

	local range = line1 ~= nil and line2 ~= nil
	local single = range and line1 == line2

	local encountered = 0

	for _, match in unnamed_blocks_query:iter_matches(root, bufnr, 0, -1) do
		local passed = not range

		for id, node in pairs(match) do
			local row1, _, row2 = node:range()

			if unnamed_blocks_query.captures[id] == "block" and not passed then
				if single and line1 - 1 > row1 and line1 - 1 < row2 then
					passed = true
				elseif not single and line1 - 1 <= row1 and row2 <= line2 - 1 then
					passed = true
				end
			end
		end

		if passed then
			table.insert(indexes, encountered)
		end

		encountered = encountered + 1
	end
	return indexes
end

vim.api.nvim_create_user_command("OrgExecute", function(el)
	local line1 = el.line1
	local line2 = el.line2
	local range = el.range
	local arg = el.args
	local bufnr = vim.api.nvim_get_current_buf()
	local filename = vim.fn.expand(vim.api.nvim_buf_get_name(bufnr), ":p")
	local total_lines = vim.api.nvim_buf_line_count(bufnr)

	if vim.bo.modified then
		vim.notify("Warning: OrgExecute in modified buffer!", vim.log.levels.WARN)
	end

	local script = M._run_all
	local parameters = {}

	local blocks = M.get_blocks_in_buffer(0, line1, line2)

	if #blocks <= 0 then
		vim.notify("No blocks", vim.log.levels.INFO)
		return
	end

	if arg ~= "" then
		if el.bang then
			vim.notify("Evaulating code block (" .. arg .. ") on your system", vim.log.levels.INFO)
		elseif
			vim.fn.input({
				prompt = "Evaulate code block (" .. arg .. ") on your system? (y/N) ",
				cancelreturn = "n",
			}) ~= "y"
		then
			return
		end

		script = M._run_by_name
		parameters = { arg }
	elseif line1 == 1 and line2 == total_lines then
		if el.bang then
			vim.notify("Evaulating all blocks on your system", vim.log.levels.INFO)
		elseif vim.fn.input({ prompt = "Evaulate all blocks on your system? (y/N) ", cancelreturn = "n" }) ~= "y" then
			return
		end
	elseif range == 1 then
		script = M._run_by_number
		local named_blocks = M.get_names_in_buffer(0, line1, line2)

		if #blocks > 1 then
			vim.notify("Bailing on multiple blocks with single line range ?? (Report upstream)", vim.log.level.ERROR)

			return
		end

		local name = ""

		if #named_blocks > 0 then
			name = "(" .. named_blocks[1] .. ") "
		end

		if el.bang then
			vim.notify("Evaulating this code block " .. name .. "on your system", vim.log.levels.INFO)
		elseif
			vim.fn.input({
				prompt = "Evaulate this code block " .. name .. "on your system? (y/N) ",
				cancelreturn = "n",
			}) ~= "y"
		then
			return
		end

		parameters = blocks
	elseif range == 2 then
		script = M._run_by_number
		local names = M.get_names_in_buffer(0, line1, line2)

		local names_string = #names > 0 and table.concat(names, ", ") or ""
		local sep = #blocks > #names and #names > 0 and ", " or ""
		local unnamed_string = #blocks > #names and #blocks - #names .. " unnamed" or ""

		if el.bang then
			vim.notify(
				"Evaulating blocks (" .. names_string .. sep .. unnamed_string .. ") on your system",
				vim.log.levels.INFO
			)
		elseif
			vim.fn.input({
				prompt = "Evaulate blocks (" .. names_string .. sep .. unnamed_string .. ") on your system? (y/N) ",
				cancelreturn = "n",
			}) ~= "y"
		then
			return
		end

		parameters = blocks
	end

	local cmd = vim.deepcopy(M._base_cmd)

	vim.list_extend(cmd, {
		"--load",
		script,
		filename,
	})

	vim.list_extend(cmd, parameters)

	-- vim.notify(cmd, vim.log.levels.DEBUG)

	local output = vim.fn.system(cmd)

	vim.print(output)

	if not vim.bo[bufnr].modified then
		vim.cmd(bufnr .. "bufdo edit")
	end
end, {
	nargs = "?",
	range = "%",
	bang = true,
	complete = function()
		return M.get_names_in_buffer()
	end,
})

vim.api.nvim_create_user_command("OrgTangle", function(el)
	local line1 = el.line1
	local line2 = el.line2
	local range = el.range
	local bufnr = vim.api.nvim_get_current_buf()
	local filename = vim.fn.expand(vim.api.nvim_buf_get_name(bufnr), ":p")
	local total_lines = vim.api.nvim_buf_line_count(bufnr)

	local script = M._tangle_all
	local parameters = {}

	local blocks = M.get_blocks_in_buffer(0, line1, line2)

	if #blocks <= 0 then
		vim.notify("No blocks", vim.log.levels.INFO)
		return
	end

	if line1 == 1 and line2 == total_lines then
		if el.bang then
			vim.notify("Tangling whole file", vim.log.levels.INFO)
		elseif vim.fn.input({ prompt = "Tangle whole file? (y/N) ", cancelreturn = "n" }) ~= "y" then
			return
		end
	elseif range == 1 then
		script = M._tangle_by_number
		local named_blocks = M.get_names_in_buffer(0, line1, line2)

		if #blocks > 1 then
			vim.notify("Bailing on multiple blocks with single line range ?? (Report upstream)", vim.log.levinitOR)

			return
		end

		local name = ""

		if #named_blocks > 0 then
			name = "(" .. named_blocks[1] .. ") "
		end

		if el.bang then
			vim.notify("Tangling this code block " .. name, vim.log.levels.INFO)
		elseif
			vim.fn.input({
				prompt = "Tangle this code block " .. name .. "? (y/N) ",
				cancelreturn = "n",
			}) ~= "y"
		then
			return
		end

		parameters = blocks
	elseif range == 2 then
		script = M._tangle_by_number
		local names = M.get_names_in_buffer(0, line1, line2)

		local names_string = #names > 0 and table.concat(names, ", ") or ""
		local sep = #blocks > #names and #names > 0 and ", " or ""
		local unnamed_string = #blocks > #names and #blocks - #names .. " unnamed" or ""

		if el.bang then
			vim.notify("Tangling blocks (" .. names_string .. sep .. unnamed_string .. ")", vim.log.levels.INFO)
		elseif
			vim.fn.input({
				prompt = "Tangle blocks (" .. names_string .. sep .. unnamed_string .. ")? (y/N) ",
				cancelreturn = "n",
			}) ~= "y"
		then
			return
		end

		parameters = blocks
	end

	local cmd = vim.deepcopy(M._base_cmd)

	vim.list_extend(cmd, {
		"--load",
		script,
		filename,
	})

	vim.list_extend(cmd, parameters)

	-- vim.notify(cmd, vim.log.levels.DEBUG)

	local output = vim.fn.system(cmd)

	vim.print(output)
end, {
	range = "%",
	bang = true,
})

return M
