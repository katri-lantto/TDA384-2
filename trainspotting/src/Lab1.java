import TSim.*;
import java.util.*;
import java.util.concurrent.*;

public class Lab1 {
  public TSimInterface tsi = TSimInterface.getInstance();

  public int[][] switchIDs = {
    {17, 7},            // 0 upper station
    {15, 9},            // 1 meeting section, east
    {4, 9},             // 2 meeting section, west
    {3, 11}             // 3 lower station
  };

  public enum SensorID {
    UPPER_STATION_HALT_A, UPPER_STATION_HALT_B,
    UPPER_STATION_SWITCH_A, UPPER_STATION_SWITCH_B,
    MEETING_EAST_A, MEETING_EAST_B,
    MEETING_WEST_A, MEETING_WEST_B,
    LOWER_STATION_SWITCH_A, LOWER_STATION_SWITCH_B,
    LOWER_STATION_HALT_A, LOWER_STATION_HALT_B,
    CROSSING_HORI_A, CROSSING_HORI_B,
    CROSSING_VERT_A, CROSSING_VERT_B
  }

  public Map<SensorID, int[]> sensorIDs;

  public Semaphore semUpperStation = new Semaphore(1);
  public Semaphore semLowerStation = new Semaphore(1);
  public Semaphore semWestCritical = new Semaphore(1);
  public Semaphore semEastCritical = new Semaphore(1);
  public Semaphore semMiddle = new Semaphore(1);
  public Semaphore semCrossing = new Semaphore(1);

  public Lab1(Integer speed1, Integer speed2) {
    initSensorIDs();

    try {
      // setSwitch(0, tsi.SWITCH_RIGHT);
      setSwitch(1, tsi.SWITCH_RIGHT);

      Train train1 = new Train(1);
      Train train2 = new Train(2);

      train1.setSpeed(speed1);
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

  private void initSensorIDs() {
    sensorIDs = new HashMap<>();

    sensorIDs.put(SensorID.UPPER_STATION_HALT_A, new int[]{13, 3});
    sensorIDs.put(SensorID.UPPER_STATION_HALT_B, new int[]{13, 5});
    sensorIDs.put(SensorID.UPPER_STATION_SWITCH_A, new int[]{15, 7});
    sensorIDs.put(SensorID.UPPER_STATION_SWITCH_B, new int[]{15, 8});

    sensorIDs.put(SensorID.MEETING_EAST_A, new int[]{13, 9});
    sensorIDs.put(SensorID.MEETING_EAST_B, new int[]{13, 10});
    sensorIDs.put(SensorID.MEETING_WEST_A, new int[]{6, 9});
    sensorIDs.put(SensorID.MEETING_WEST_B, new int[]{6, 10});

    sensorIDs.put(SensorID.LOWER_STATION_SWITCH_A, new int[]{5, 11});
    sensorIDs.put(SensorID.LOWER_STATION_SWITCH_B, new int[]{4, 13});
    sensorIDs.put(SensorID.LOWER_STATION_HALT_A, new int[]{13, 11});
    sensorIDs.put(SensorID.LOWER_STATION_HALT_B, new int[]{13, 13});

    sensorIDs.put(SensorID.CROSSING_HORI_A, new int[]{6, 7});
    sensorIDs.put(SensorID.CROSSING_HORI_B, new int[]{10, 7});
    sensorIDs.put(SensorID.CROSSING_VERT_A, new int[]{8, 5});
    sensorIDs.put(SensorID.CROSSING_VERT_B, new int[]{10, 8});
  }

  private void setSwitch(int switchID, int switchDir) throws CommandException {
    int[] switchPos = switchIDs[switchID];
    tsi.setSwitch(switchPos[0], switchPos[1], switchDir);
  }

  private boolean isSensor(SensorEvent sensorEvent, SensorID sensorID) {
    int[] sensorPos = sensorIDs.get(sensorID);
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
          if (s.getStatus() == s.ACTIVE
                && (isSensor(s, SensorID.UPPER_STATION_HALT_A)
                || isSensor(s, SensorID.UPPER_STATION_HALT_B)
                || isSensor(s, SensorID.LOWER_STATION_HALT_A)
                || isSensor(s, SensorID.LOWER_STATION_HALT_B))
                && !atStation) {
            atStation = true;
            int previousSpeed = speed;
            setSpeed(0);
            Thread.sleep(1000 + (20 * Math.abs(previousSpeed)));
            setSpeed(-previousSpeed);

          } else if (s.getStatus() == s.ACTIVE
                && (isSensor(s, SensorID.UPPER_STATION_SWITCH_A)
                || isSensor(s, SensorID.UPPER_STATION_SWITCH_B)
                || isSensor(s, SensorID.LOWER_STATION_SWITCH_A)
                || isSensor(s, SensorID.LOWER_STATION_SWITCH_B))
                && atStation) {
            atStation = false;
          }

          // *** Railway crossing ***
          if (s.getStatus() == s.ACTIVE
                && (isSensor(s, SensorID.CROSSING_HORI_A)
                || isSensor(s, SensorID.CROSSING_HORI_B)
                || isSensor(s, SensorID.CROSSING_VERT_A)
                || isSensor(s, SensorID.CROSSING_VERT_B) )) {
            if ((atStation && (isSensor(s, SensorID.CROSSING_HORI_A)
                  || isSensor(s, SensorID.CROSSING_VERT_A)))
                  || (!atStation && (isSensor(s, SensorID.CROSSING_HORI_B)
                  || isSensor(s, SensorID.CROSSING_VERT_B)))   ) {
              int previousSpeed = speed;
              setSpeed(0);
              semCrossing.acquire();
              setSpeed(previousSpeed);
            } else {
              semCrossing.release();
            }
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
