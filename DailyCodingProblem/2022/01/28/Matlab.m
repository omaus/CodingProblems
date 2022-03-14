function result = multiplyNotSelf(list)
len = size(list,2);
result = zeros(1,len);
for i = 1 : len
    acc = 1;
    for j = 1 : len
        if j ~= i
            acc = acc * list(1,j);
        end
        result(1,i) = acc;
    end
end