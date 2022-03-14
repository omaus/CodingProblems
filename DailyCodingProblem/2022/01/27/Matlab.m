function result = doesAddUp(listOfNumbers, k)
check = false;
i1 = 0;
i2 = 0;
while check == false && i1 < size(listOfNumbers,2)
    i1 = i1 + 1;
    while check == false && i2 < size(listOfNumbers,2)
        i2 = i2 + 1;
        if listOfNumbers(i1) + listOfNumbers(i2) == k
            check = true;
        end
    end
end
result = check;