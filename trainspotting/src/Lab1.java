import TSim.*;
import java.util.concurrent.*;

public class Lab1 {
  public TSimInterface tsi = TSimInterface.getInstance();

  public int[][] switchIDs = {
    {17, 7},            // 0 upper station
    {15, 9},            // 1 meeting section, east
    {4, 9},             // 2 meeting section, west
    {3, 11}             // 3 lower station
  };

  public int[][] sensorIDs = {
    {13, 3}, {13, 5},   // 0,1 upper station halt
    {15, 7}, {15, 8},   // 2,3 upper station switch
    {18, 7}, {16, 9},   // 4,5 east critical section
    {14, 9}, {14, 10},  // 6,7 meeting section, east switch
    {5, 9}, {5, 10},    // 8,9 meeting section, west switch
    {3, 9}, {2, 11},    // 10,11 west critical section
    {5, 11}, {4, 13},   // 12,13 lower station switch
    {13, 11}, {13, 13}  // 14,15 lower station halt
  };

  public Semaphore semUpperStation = new Semaphore(1);
  public Semaphore semLowerStation = new Semaphore(1);
  public Semaphore semWestCritical = new Semaphore(1);
  public Semaphore semEastCritical = new Semaphore(1);
  public Semaphore semMiddle = new Semaphore(1);

  public Lab1(Integer speed1, Integer speed2) {
    try {
      // setSwitch(0, tsi.SWITCH_RIGHT);
      setSwitch(1, tsi.SWITCH_RIGHT);

      // tsi.setSpeed(1,speed1);
      // tsi.setSpeed(2,speed2);

      Train train1 = new Train(1);
      Train train2 = new Train(2);

      // train1.setSpeed(speed1);
      train2.setSpeed(speed2);

      Thread trainThread1 = new Thread(train1);
      Thread trainThread2 = new Thread(train2);

      trainThread1.start();
      trainThread2.start();
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
    private int speed;
    private boolean atStation;

    public Train (int id) {
      this.id = id;
      atStation = true;
    }

    public void setSpeed(int speed) throws CommandException {
      this.speed = speed;
      tsi.setSpeed(id,speed);
    }

    public void run() {
      try {

        while (true) {
          SensorEvent s = tsi.getSensor(id);

          // *** Stopping at stations ***
          if ((isSensor(s, 0) || isSensor(s, 1) ||
                isSensor(s, 14) || isSensor(s, 15)) && !atStation) {
            atStation = true;
            int previousSpeed = speed;
            setSpeed(0);
            Thread.sleep(1000 + (20 * Math.abs(previousSpeed)));
            setSpeed(-previousSpeed);

          } else if ((isSensor(s, 2) || isSensor(s, 3)
                || isSensor(s, 12) || isSensor(s, 13)) && atStation) {
            atStation = false;
          }

          
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
