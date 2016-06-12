package com.example.home.ttttest;

/**
 * Created by Home on 2015-07-18.
 */
public class Storage {
    private Store accel;
    private Store gyro;

    Storage() {
    }

    Storage(Store accel, Store gyro) {
        this.accel = accel;
        this.gyro = gyro;
    }

    @Override
    public String toString() {
        return accel.toString() + "," + gyro.toString() + "\n";
    }
}

class Store {
    private String x;
    private String y;
    private String z;
    private String ts;

    Store() {
    }

    Store(float x, float y, float z, long ts) {
        this.x = String.valueOf(x);
        this.y = String.valueOf(y);
        this.z = String.valueOf(z);
        this.ts = String.valueOf(ts);
    }

    @Override
    public String toString() {
        return x + "," + y + "," + z + "," + ts;
    }
}
