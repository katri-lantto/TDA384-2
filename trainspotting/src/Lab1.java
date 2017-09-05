import TSim.*;

public class Lab1 {
  public TSimInterface tsi = TSimInterface.getInstance();

  public int[][] switchIDs = {
    {17, 7},            // upper station
    {15, 9},            // meeting section, east
    {4, 9},             // meeting section, west
    {3, 11}             // lower station
  };

  public int[][] sensorIDs = {
    {13, 3}, {13, 5},   // upper station halt
    {15, 7}, {15, 8},   // upper station switch
    {18, 7}, {16, 9},   // east critical section
    {14, 9}, {14, 10},  // meeting section, east switch
    {5, 9}, {5, 10},    // meeting section, west switch
    {3, 9}, {2, 11},    // west critical section
    {5, 11}, {4, 13},   // lower station switch
    {13, 11}, {13, 13}  // lower station halt
  };

  public Lab1(Integer speed1, Integer speed2) {
    try {
      setSwitch(0, tsi.SWITCH_RIGHT);
      setSwitch(1, tsi.SWITCH_RIGHT);

      // tsi.setSpeed(1,speed1);
      tsi.setSpeed(2,speed2);

      Thread trainOne = new Thread(new Train(1));
      Thread trainTwo = new Thread(new Train(2));
      trainOne.start();
      trainTwo.start();
    }
    catch (CommandException e) {
      e.printStackTrace();    // or only e.getMessage() for the error
      System.exit(1);
    }
  }

  private void setSwitch(int switchID, int switchDir) throws CommandException {
    int[] switchPos = switchIDs[switchID];
    tsi.setSwitch(switchPos[0], switchPos[1], switchDir);
  }

  private boolean isSensor(SensorEvent sensorEvent, int sensorID) {
    int[] sensorPos = sensorIDs[sensorID];
    return (sensorEvent.getXpos() == sensorPos[0]
      && sensorEvent.getYpos() == sensorPos[1]);
  }


  class Train implements Runnable {
    private int id;

    public Train (int id) {
      this.id = id;
    }

    public void run() {
      try {

        while (true) {
          SensorEvent s1 = tsi.getSensor(id);
          // System.out.println("is sensor 0: "+isSensor(s1, 0));

        }
      } catch (CommandException e) {
        e.printStackTrace();
        System.exit(1);
      }catch (InterruptedException e) {
        e.printStackTrace();
        System.exit(1);
      }
    }
  }
}
