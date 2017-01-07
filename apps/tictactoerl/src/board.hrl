-define(NO_SELECTION, " ").

-record(board, {top_left    = ?NO_SELECTION :: string(), top_center    = ?NO_SELECTION :: string(), top_right    = ?NO_SELECTION :: string(), 
                mid_left    = ?NO_SELECTION :: string(), center        = ?NO_SELECTION :: string(), mid_right    = ?NO_SELECTION :: string(), 
                bottom_left = ?NO_SELECTION :: string(), bottom_center = ?NO_SELECTION :: string(), bottom_right = ?NO_SELECTION :: string()}).

-type board() :: #board{}.

-export_type([board/0]).
