package s2_144.nozhkin.test2_1;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Alert;
import javafx.stage.Stage;

public class Main extends Application {

    @Override
    public void start(Stage primaryStage) throws Exception {
        FXMLLoader loader = new FXMLLoader(getClass().getClassLoader().getResource("layout.fxml"));
        Parent root = loader.load();

        Controller controller = loader.getController();
        if (!controller.isInitialized()) {
            Alert alert = new Alert(Alert.AlertType.ERROR);
            alert.setTitle("Find a pair");
            alert.setHeaderText("Field size is incorrect");
            alert.show();
            return;
        }

        primaryStage.setTitle("Find a pair");
        primaryStage.setScene(new Scene(root, Controller.FORM_SIZE, Controller.FORM_SIZE));
        primaryStage.setResizable(false);

        primaryStage.show();
    }

    public static void main(String[] args) {
        launch(args);
    }
}
