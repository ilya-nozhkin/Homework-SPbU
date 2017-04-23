package s2_144.nozhkin.task7_2;

/** class that provides a basic tic-tac-toe game functionality */
public class Game {
    /** game area - 9 cells that may contain 'O' if it is O, 'X' if it is X or 0 if it is empty */
    private char[][] area = new char[3][3];

    /** true if next cell should be marked with X, false if O */
    private boolean turn = true;

    /**
     * marks selected cell with X or O depending on turn
     *
     * @param x X coordinate
     * @param y Y coordinate
     * @return 'O' if it has been marked with O, 'X' if with X and 0 if it has not been marked
     */
    public char markCell(int x, int y) {
        if (area[y][x] != 0) {
            return 0;
        }

        area[y][x] = turn ? 'X' : 'O';
        turn = !turn;
        return area[y][x];
    }
}
