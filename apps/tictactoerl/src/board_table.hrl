-define(NO_SELECTION, " ").

-record(board_table, {top_left    = ?NO_SELECTION, top_center    = ?NO_SELECTION, top_right    = ?NO_SELECTION, 
                      mid_left    = ?NO_SELECTION, center        = ?NO_SELECTION, mid_right    = ?NO_SELECTION, 
                      bottom_left = ?NO_SELECTION, bottom_center = ?NO_SELECTION, bottom_right = ?NO_SELECTION}).
