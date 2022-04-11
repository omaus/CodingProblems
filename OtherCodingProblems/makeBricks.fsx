// Task:
// We want to make a row of bricks that is `goal` inches long.
// We have a number of small bricks (1 inch each) and big bricks (5 inches each). 
// Return true if it is possible to make the goal by choosing from the given bricks. 
// Hint: This is a little harder than it looks and can be done without any loops.
// taken from: https://codingbat.com/doc/practice/makebricks-introduction.html

let makeBricks goal numSmallBricks numBigBricks =
    if goal < 0 then failwith "Only positive numbers (and 0) are allowed."
    goal % 5 <= numSmallBricks // we must be able to fill out with small bricks if any big bricks were used
    && // both conditions must be true
    goal - 5 * numBigBricks <= numSmallBricks // we must be able to fill out with small bricks if all big bricks were used

makeBricks 7 2 2
makeBricks 8 2 2
makeBricks 12 2 2
makeBricks 20 2 2