function newObject(object, selfptr)
    setmetatable(object, selfptr)
    selfptr.__index = selfptr
    return object
end

TreeNode = {}
function TreeNode:new(key_, value_, comparator_)
    local object = {
        left = nil,
        right = nil,
        key = key_,
        value = value_,
        comparator = comparator_
    }

    function object:add(key, value)
        local position = object.comparator(key, object.key)

        if position == 0 then
            return false
        end

        if position < 0 then
            if object.left then
                return object.left:add(key, value)
            else
                object.left = TreeNode:new(key, value, object.comparator)
            end
        else
            if object.right then
                return object.right:add(key, value)
            else
                object.right = TreeNode:new(key, value, object.comparator)
            end
        end

        return true
    end

    function object:extractMostRight()
        if not object.right then
            return object, object.left
        end 

        local current = object
        while current.right.right do
            current = current.right
        end

        local result = current.right
        current.right = result.left
        return result, object
    end

    local function removeIfKeyFound()
        if not (object.left and object.right) then
            return true, object.left or object.right
        else
            local mostRight, newLeft = object.left:extractMostRight()
            mostRight.left = newLeft
            mostRight.right = object.right
            return true, mostRight
        end
    end

    local function removeIfKeyNotFound(position, key)
        if position < 0 then
            if not object.left then
                return false, object
            end

            local status, newLeft = object.left:remove(key)
            object.left = newLeft
            return status, object
        else
            if not object.right then
                return false, object
            end

            local status, newRight = object.right:remove(key)
            object.right = newRight 
            return status, object
        end
    end

    function object:remove(key)
        local position = object.comparator(key, object.key)

        if position == 0 then
            return removeIfKeyFound()
        end

        return removeIfKeyNotFound(position, key)
    end

    function object:find(key)
        local position = object.comparator(key, object.key)

        if position == 0 then
            return object.value, true
        elseif position < 0 then
            if not object.left then
                return nil, false
            end 
            return object.left:find(key)
        else
            if not object.right then
                return nil, false
            end
            return object.right:find(key)
        end
    end

    return newObject(object, self) 
end

Tree = {}
function Tree:new(comparator_)
    local private = {
        root = nil,
        comparator = comparator_
    }

    local public = {}

    function public:add(key, value)
        if private.root then
            return private.root:add(key, value)
        else
            private.root = TreeNode:new(key, value, private.comparator)
            return true
        end
    end

    function public:remove(key)
        if private.root then
            local status, newNode = private.root:remove(key)
            private.root = newNode
            return status
        end
        return false
    end

    function public:find(key)
        if not private.root then
            return nil, false
        end
        return private.root:find(key)
    end

    -- Iterator.
    function public:deepFirstSearchIterator()
        local stack = {}
    
        function deepIntoLeft(node)
            local current = node
            while current do
                table.insert(stack, current)
                current = current.left
            end
        end
    
        deepIntoLeft(private.root)
    
        return function()
            if #stack > 0 then
                local node = stack[#stack]
                table.remove(stack, #stack)
    
                if node.right then
                    deepIntoLeft(node.right)
                end
    
                return node.key, node.value
            end
        end
    end

    return newObject(public, self)
end

function compareNumbers(left, right)
    return (left < right) and -1 or ((left > right) and 1 or 0)
end

function testIterationOrder()
    local tree = Tree:new(compareNumbers)

    local size = 500
    local data = {}
    for i = 1, size, 1 do
        table.insert(data, math.random(size))
    end

    local distinct = {}
    for i, key in ipairs(data) do
        if tree:add(key) then
            table.insert(distinct, key)
        end
    end

    while #distinct > 0 do
        local index = math.random(1, #distinct)
        local key = distinct[index]
        table.remove(distinct, index)

        tree:remove(key)

        local previous = -1
        for key in tree:deepFirstSearchIterator() do
            if key < previous then
                return false
            end
            previous = key
        end
    end

    return true
end

if not testIterationOrder() then
    print("Iteration order test failed.")
else
    tree = Tree:new(compareNumbers)

    print("Add some elements")
    tree:add(50, "a")
    tree:add(20, true)
    tree:add(30, 10)
    tree:add(60, {'t', 'a', 'b', 'l', 'e'})

    for key, value in tree:deepFirstSearchIterator() do
        print(key, value)
    end

    print("Remove key 20")
    tree:remove(20)

    for key, value in tree:deepFirstSearchIterator() do
        print(key, value)
    end

    print("Find value associated with key 30: " .. tostring(tree:find(30)))
end
