type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState deriving (Show,Eq)

up:: MyState -> MyState
up (S (0,y) l s a) = Null
up (S (x,y) cells s mystate) = S (x-1,y) cells  "up" (S (x,y) cells s mystate)

down (S (3,y) l s a) = Null
down (S (x,y) cells s mystate) = S (x+1,y) cells  "down" (S (x,y) cells s mystate)

left (S (x,0) l s a) = Null
left (S (x,y) cells s mystate) = S (x,y-1) cells  "left" (S (x,y) cells s mystate)

right (S (x,3) l s a) = Null
right (S (x,y) cells s mystate) = S (x,y+1) cells  "right" (S (x,y) cells s mystate)

collectH (x1,y1) (x2,y2) = if (x1 == x2 && y1==y2) then True else False
checkAvailable l [] = False
checkAvailable l (h:t) = if (collectH l h) then True else checkAvailable l t
collectHL l [] = []
collectHL l (h:t) = if (collectH l h == True) then t else h:(collectHL l t)
collect (S (x,y) (h:t) s mystate) = if (checkAvailable (x,y) (h:t)) then S (x,y) nl "collect" (S (x,y) (h:t) s mystate) else Null
									where nl = collectHL (x,y) (h:t)

nextH mystate = [up mystate , down mystate , left mystate , right mystate , collect mystate]
removeNull [] = []
removeNull (h:t) = if (h == Null) then removeNull t else h : removeNull t 
nextMyStates mystate = removeNull a where a = nextH mystate

isGoal (S (x,y) [] s mystate) = True
isGoal (S (x,y) l s a ) = False

search (h:t) = if (isGoal h) then h else search (t ++ a) where a = nextMyStates h
constructSolution Null = []
constructSolution (S (x1,y1) l s Null) = []
constructSolution (S (x,y) l s mystate) = (a ++ [s]) where a = constructSolution mystate

solve cell cells = constructSolution (search(nextMyStates a)) where a = (S cell cells "" Null)