package s2_144.nozhkin.task7_2;

import java.util.Arrays;

/** class that provides a basic tic-tac-toe game functionality */
public class Game {
    /** width and height of an area */
    static final int AREA_SIZE = 3;

    /** game area - AREA_SIZE^2 cells that may contain 'O' if it is O, 'X' if it is X or 0 if it is empty */
    private char[][] area = new char[AREA_SIZE][AREA_SIZE];

    /** true if next cell should be marked with X, false if O */
    private boolean turn = true;

    /** flag which indicates that game is finished */
    private char finished;

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

        finished = checkCurrentState(x, y);

        return area[y][x];
    }

    /**
     * checks if last turn finishes the game
     *
     * @param lastX X coordinate of the last mark
     * @param lastY Y coordinate of the last mark
     * @return 0 if game is still going on, 'X' if game is finished and X has won and 'Y' if Y has won
     */
    private char checkCurrentState(int lastX, int lastY) {
        boolean result = checkLine(lastX, lastY, 1,  0) ||
                         checkLine(lastX, lastY, 0,  1) ||
                         checkLine(lastX, lastY, 1,  1) ||
                         checkLine(lastX, lastY, 1, -1);

        return result ? area[lastY][lastX] : 0;
    }

    /**
     * checks that all cells are filled and returns game result
     *
     * @return 0 if game is still going on, 'X' if game is finished and X has won and 'Y' if Y has won
     */
    public char checkIfFinished() {
        return finished;
    }

    /**
     * checks that line is filled with same symbols
     *
     * @param startX X coordinate of the initial position
     * @param startY Y coordinate of the initial position
     * @param stepX X coordinate of the direction
     * @param stepY Y coordinate of the direction
     * @return true if line is filled with same symbols
     */
    private boolean checkLine(int startX, int startY, int stepX, int stepY) {
        char checkee = area[startY][startX];
        if (checkee == 0) {
            return false;
        }

        int checked = checkForward(checkee, startX, startY,  stepX,  stepY) +
                      checkForward(checkee, startX, startY, -stepX, -stepY);
        return checked == AREA_SIZE - 1;
    }

    /**
     * checks that line is filled with same symbols but only in one direction
     *
     * @param checkee symbol which other symbols compares with
     * @param startX X coordinate of the initial position
     * @param startY Y coordinate of the initial position
     * @param stepX X coordinate of the direction
     * @param stepY Y coordinate of the direction
     * @return number of checked symbols if they all same and 0 if they are not
     */
    private int checkForward(char checkee, int startX, int startY, int stepX, int stepY) {
        int x = startX;
        int y = startY;

        int checked = 0;
        while (x >= 0 && x < AREA_SIZE &&
               y >= 0 && y < AREA_SIZE) {
            if (area[y][x] != checkee) {
                return 0;
            }

            x += stepX;
            y += stepY;
            checked++;
        }

        return checked - 1;
    }
}
