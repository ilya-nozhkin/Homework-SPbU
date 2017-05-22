package s2_144.nozhkin.test2_1;

import javafx.application.Platform;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Alert;
import javafx.scene.control.Button;
import javafx.scene.layout.Pane;

import java.net.URL;
import java.util.*;

/** controller that provides the 'Find a pair' game realization */
public class Controller implements Initializable {
    /** number of cells on width and height (N) */
    private static final int FIELD_SIZE = 4;

    /** max possible number */
    private static final int MAX_NUMBER = 100;

    /** size of cells */
    private static final int BUTTON_SIZE = 40;

    /** offset between cells */
    private static final int BUTTON_OFFSET = 10;

    /** size of the form in dots */
    public static final int FORM_SIZE = BUTTON_SIZE * FIELD_SIZE + BUTTON_OFFSET * (FIELD_SIZE + 1);

    /** values that are hidden in cells */
    private int[][] cellValues = new int[FIELD_SIZE][FIELD_SIZE];

    /** cell buttons */
    private Button[][] cells = new Button[FIELD_SIZE][FIELD_SIZE];

    /** flag that indicates that first button has been pressed but second has not pressed yet*/
    private boolean firstPressedFlag = false;

    /** coordinates of first pressed button */
    private int firstPressedI;
    private int firstPressedJ;

    /** flag whish indicates that field is locked and you can't push buttons because two cells are open*/
    private boolean fieldLocked = false;

    /** number of hidden values */
    private int remaining = 0;

    /** flag which shows that game is initialized successfully */
    private boolean initialized = false;

    /** pane that contains cells */
    @FXML
    private Pane gameField;

    public void initialize(URL location, ResourceBundle resources) {
        if (FIELD_SIZE % 2 != 0 || FIELD_SIZE == 0) {
            initialized = false;
            return;
        }

        Random generator = new Random();
        List<Integer> values = new ArrayList<>();

        gameField.setPrefWidth(FORM_SIZE);
        gameField.setPrefHeight(FORM_SIZE);

        for (int i = 0; i < FIELD_SIZE * FIELD_SIZE / 2; i++) {
            values.add(generator.nextInt(MAX_NUMBER));
        }

        values.addAll(values);

        for (int i = 0; i < FIELD_SIZE; i++) {
            for (int j = 0; j < FIELD_SIZE; j++) {
                int id = generator.nextInt(values.size());
                cellValues[i][j] = values.get(id);
                values.remove(id);

                Button cell = new Button();
                cell.setPrefWidth(BUTTON_SIZE);
                cell.setPrefHeight(BUTTON_SIZE);
                cell.setLayoutX(BUTTON_OFFSET * (j + 1) + BUTTON_SIZE * j);
                cell.setLayoutY(BUTTON_OFFSET * (i + 1) + BUTTON_SIZE * i);

                CellHandler handler = new CellHandler();
                handler.x = j;
                handler.y = i;

                cell.setOnAction(handler);

                cells[i][j] = cell;
                gameField.getChildren().add(cell);
            }
        }

        remaining = FIELD_SIZE * FIELD_SIZE / 2;
        initialized = true;
    }

    /**
     * checks that game is initialized successfully
     * @return true if game is initialized
     */
    public boolean isInitialized() {
        return initialized;
    }

    /**
     * processes game turn.
     * checks if two buttons are pressed and compares their values
     * if only one button is pressed then stores its coordinates
     *
     * @param i y coordinate of last pressed button
     * @param j x coordinate of last pressed button
     */
    private void gameAction(int i, int j) {
        if (!firstPressedFlag) {
            firstPressedI = i;
            firstPressedJ = j;
        } else {
            int firstValue = cellValues[firstPressedI][firstPressedJ];
            int secondValue = cellValues[i][j];

            if (firstValue == secondValue) {
                cellValues[firstPressedI][firstPressedJ] = -1;
                cellValues[i][j] = -1;

                remaining--;

                if (remaining == 0) {
                    gameIsWon();
                }
            } else {
                cleanPressed(firstPressedI, firstPressedJ, i, j);
            }
        }

        firstPressedFlag = !firstPressedFlag;
    }

    /** shows a message box with information that game is won */
    private void gameIsWon() {
        Alert alert = new Alert(Alert.AlertType.INFORMATION);
        alert.setTitle("Find a pair");
        alert.setHeaderText("You have won!!!");
        alert.show();
    }

    /**
     * locks game field, waits one second and hides opened cells
     *
     * @param firstI y coordinate of the first button
     * @param firstJ x coordinate of the first button
     * @param secondI y coordinate of the second button
     * @param secondJ x coordinate of the second button
     */
    private void cleanPressed(int firstI, int firstJ, int secondI, int secondJ) {
        fieldLocked = true;

        Thread cleaningThread = new Thread() {
            @Override
            public void run() {
                try {
                    Thread.sleep(1000);
                } catch (InterruptedException e) {}

                Platform.runLater(new Runnable() {
                    @Override
                    public void run() {
                        cells[firstI][firstJ].setText("");
                        cells[secondI][secondJ].setText("");
                    }
                });

                fieldLocked = false;
            }
        };

        cleaningThread.start();
    }

    /** cell pressed event handler (shows the value of cell and calls game process method) */
    private class CellHandler implements EventHandler<ActionEvent> {
        /** coordinates of operated cell */
        int x;
        int y;

        @Override
        public void handle(ActionEvent event) {
            if (fieldLocked) {
                return;
            }

            boolean pressed = firstPressedFlag && firstPressedI == y && firstPressedJ == x;
            if (!pressed && cellValues[y][x] > 0) {
                cells[y][x].setText(Integer.toString(cellValues[y][x]));
                gameAction(y, x);
            }
        }
    }
}
