package s2_144.nozhkin.task7_2;

import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;

import java.net.URL;
import java.util.ArrayList;
import java.util.ResourceBundle;

/** JavaFX controller that provides event handlers to buttons */
public class Controller implements Initializable {
    /** game state object */
    private Game game = new Game();

    /** list of cell buttons */
    @FXML
    private ArrayList<Button> cellsList;

    /**
     * event handler for restart button
     *
     * @param actionEvent event information
     */
    public void restart(ActionEvent actionEvent) {
        game = new Game();

        for (Button cell : cellsList) {
            cell.setText("");
        }
    }

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        for (Button cell : cellsList) {
            int cellId = Integer.valueOf(cell.getId().split("cell")[1]);
            final int x = cellId % 3;
            final int y = cellId / 3;

            cell.setOnAction(event -> {
                char result = game.markCell(x, y);
                if (result != 0) {
                    cell.setText(Character.toString(result));
                }
            });
        }
    }
}
