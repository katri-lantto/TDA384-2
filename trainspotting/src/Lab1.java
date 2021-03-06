/**
*
* TDA384 - Lab 1 - Java code
* 
* Authors: Robin Lilius-Lundmark (CID: lurobin)
*          Andreas Carlsson (CID: andrc)
*
*/


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
      Train train1 = new Train(1);
      Train train2 = new Train(2);

      train1.setSpeed(speed1);
      train2.setSpeed(speed2);

      Thread trainThread1 = new Thread(train1);
      Thread trainThread2 = new Thread(train2);

      trainThread1.start();
      trainThread2.start();

      semLowerStation.acquire(); // Since train 2 allready is there
    }
    catch (CommandException e) {
      e.printStackTrace();    // or only e.getMessage() for the error
      System.exit(1);
    }catch (InterruptedException e) {
      e.printStackTrace();
      System.exit(1);
    }
  }

  private void initSensorIDs() {
    sensorIDs = new HashMap<>();

    sensorIDs.put(SensorID.UPPER_STATION_HALT_A, new int[]{14, 3});
    sensorIDs.put(SensorID.UPPER_STATION_HALT_B, new int[]{14, 5});
    sensorIDs.put(SensorID.UPPER_STATION_SWITCH_A, new int[]{14, 7});
    sensorIDs.put(SensorID.UPPER_STATION_SWITCH_B, new int[]{15, 8});

    sensorIDs.put(SensorID.MEETING_EAST_A, new int[]{12, 9});
    sensorIDs.put(SensorID.MEETING_EAST_B, new int[]{13, 10});
    sensorIDs.put(SensorID.MEETING_WEST_A, new int[]{7, 9});
    sensorIDs.put(SensorID.MEETING_WEST_B, new int[]{6, 10});

    sensorIDs.put(SensorID.LOWER_STATION_SWITCH_A, new int[]{6, 11});
    sensorIDs.put(SensorID.LOWER_STATION_SWITCH_B, new int[]{4, 13});
    sensorIDs.put(SensorID.LOWER_STATION_HALT_A, new int[]{14, 11});
    sensorIDs.put(SensorID.LOWER_STATION_HALT_B, new int[]{14, 13});

    sensorIDs.put(SensorID.CROSSING_HORI_A, new int[]{6, 6});
    sensorIDs.put(SensorID.CROSSING_HORI_B, new int[]{11, 7});
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
    private boolean inMiddle;

    public Train (int id) {
      this.id = id;
      this.atStation = true;
      this.inMiddle = false;
    }

    public void setSpeed(int speed) throws CommandException {
      this.speed = speed;
      tsi.setSpeed(this.id, this.speed);
    }

    private void criticalSemaphoreAcquirement(Semaphore semToAcquire, Semaphore semToRelease)
        throws InterruptedException, CommandException {
      int previousSpeed = this.speed;
      setSpeed(0);
      semToAcquire.acquire();
      setSpeed(previousSpeed);
      if (semToRelease.availablePermits() == 0) {
        semToRelease.release();
      }
    }

    public void run() {
      try {

        while (true) {
          SensorEvent s = tsi.getSensor(this.id);

          // *** West critical region - lower station switch ***
          if (s.getStatus() == s.ACTIVE
                && (isSensor(s, SensorID.LOWER_STATION_SWITCH_A)
                || isSensor(s, SensorID.LOWER_STATION_SWITCH_B))) {

            if (this.atStation) {
              this.criticalSemaphoreAcquirement(semWestCritical, semLowerStation);

              setSwitch(2, semMiddle.tryAcquire() ?
                tsi.SWITCH_LEFT : tsi.SWITCH_RIGHT);

              setSwitch(3, isSensor(s, SensorID.LOWER_STATION_SWITCH_A) ?
                tsi.SWITCH_LEFT : tsi.SWITCH_RIGHT);

              this.atStation = false;

            } else {
              semWestCritical.release();
            }
          }

          // *** West critical region - middle west switch ***
          if (s.getStatus() == s.ACTIVE
                && (isSensor(s, SensorID.MEETING_WEST_A)
                || isSensor(s, SensorID.MEETING_WEST_B))) {

            if (this.inMiddle) {
              this.criticalSemaphoreAcquirement(semWestCritical, semMiddle);

              setSwitch(3, semLowerStation.tryAcquire() ?
                tsi.SWITCH_LEFT : tsi.SWITCH_RIGHT);

              setSwitch(2, isSensor(s, SensorID.MEETING_WEST_A) ?
                tsi.SWITCH_LEFT : tsi.SWITCH_RIGHT);

              this.inMiddle = false;

            } else {
              semWestCritical.release();
              this.inMiddle = true;
            }
          }

          // *** East critical region - middle east switch ***
          if (s.getStatus() == s.ACTIVE
                && (isSensor(s, SensorID.MEETING_EAST_A)
                || isSensor(s, SensorID.MEETING_EAST_B))) {

            if (this.inMiddle) {
              this.criticalSemaphoreAcquirement(semEastCritical, semMiddle);

              setSwitch(0, semUpperStation.tryAcquire() ?
                tsi.SWITCH_LEFT : tsi.SWITCH_RIGHT);

              setSwitch(1, isSensor(s, SensorID.MEETING_EAST_A) ?
                tsi.SWITCH_RIGHT : tsi.SWITCH_LEFT);

              this.inMiddle = false;

            } else {
              semEastCritical.release();
              this.inMiddle = true;
            }
          }

          // *** East critical region - upper station switch ***
          if (s.getStatus() == s.ACTIVE
                && (isSensor(s, SensorID.UPPER_STATION_SWITCH_A)
                || isSensor(s, SensorID.UPPER_STATION_SWITCH_B))) {

            if (this.atStation) {
              this.criticalSemaphoreAcquirement(semEastCritical, semUpperStation);

              setSwitch(1, semMiddle.tryAcquire() ?
                tsi.SWITCH_RIGHT : tsi.SWITCH_LEFT);

              setSwitch(0, isSensor(s, SensorID.UPPER_STATION_SWITCH_A) ?
                tsi.SWITCH_RIGHT : tsi.SWITCH_LEFT);

              this.atStation = false;

            } else {
              semEastCritical.release();
            }
          }

          // *** Stopping at stations ***
          if (s.getStatus() == s.ACTIVE
                && (isSensor(s, SensorID.UPPER_STATION_HALT_A)
                || isSensor(s, SensorID.UPPER_STATION_HALT_B)
                || isSensor(s, SensorID.LOWER_STATION_HALT_A)
                || isSensor(s, SensorID.LOWER_STATION_HALT_B))
                && !atStation) {
            this.atStation = true;
            int previousSpeed = speed;
            setSpeed(0);
            Thread.sleep(1000 + (20 * Math.abs(previousSpeed)));
            setSpeed(-previousSpeed);
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
