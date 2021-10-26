{
module Token where
}

%wrapper "basic"

token :-
    [^\>\<\+\-\.\,\[\]]      ;
    \>                       { const MoveRight }
    \<                       { const MoveLeft }
    \+                       { const Incr }
    \-                       { const Decr }
    \.                       { const Print }
    \,                       { const Read }
    \[                       { const JumpForward }
    \]                       { const JumpBackward }

{

-- | Brainfuck token
data Token =
    -- | Increment the current cell
    Incr
    -- | Decrement the current cell
  | Decr
    -- | Move the pointer to the left
  | MoveLeft
    -- | Move the pointer to the right
  | MoveRight
    -- | Print the current cell
  | Print
    -- | Read a character and store it in the current cell
  | Read
    -- | If the current cell is zero, jump to the given token
  | JumpForward
    -- | If the current cell is non-zero, jump to the given token
  | JumpBackward
    deriving (Eq, Show)

}
