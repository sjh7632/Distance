package com.example.home.ttttest;

import android.app.Activity;
import android.hardware.Sensor;
import android.hardware.SensorEvent;
import android.hardware.SensorEventListener;
import android.hardware.SensorManager;
import android.os.Bundle;
import android.os.Environment;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.EditText;
import android.widget.Spinner;
import android.widget.Toast;

import java.io.File;
import java.io.FileOutputStream;
import java.text.DateFormat;
import java.util.Iterator;
import java.util.LinkedList;

public class MainActivity extends Activity implements AdapterView.OnItemSelectedListener {
    private SensorManager sm;
    private Sensor accelSensor;
    private Sensor gyroSensor;
    private SensorEventListener accelListener;
    private SensorEventListener gyroListener;
    private EditText edit_position;
    private EditText edit_motion;
    private EditText edit_sequence;
    private Spinner spinner_delay;
    private Button btn_reset;
    private Button btn_start;
    private ArrayAdapter<String> adapter_delay;
    private String items[] = {"DELAY_FASTEST", "DELAY_GAME", "DELAY_NORMAL", "DELAY_UI"};
    private int delay = SensorManager.SENSOR_DELAY_NORMAL;
    private boolean btn_set = true;
    private LinkedList<Storage> list;
    private LinkedList<Store> accelList;
    private LinkedList<Store> gyroList;
    private Long starttime;
    private Long endtime;


    public MainActivity() {
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        edit_position = (EditText) findViewById(R.id.editPosition);
        edit_motion = (EditText) findViewById(R.id.editMotion);
        edit_sequence = (EditText) findViewById(R.id.editSequence);
        spinner_delay = (Spinner) findViewById(R.id.spinnerDelay);
        btn_reset = (Button) findViewById(R.id.btnReset);
        btn_start = (Button) findViewById(R.id.btnStart);

        accelListener = new AccelListener();
        gyroListener = new GyroListener();

        list = new LinkedList<Storage>();
        accelList = new LinkedList<Store>();
        gyroList = new LinkedList<Store>();

        sm = (SensorManager) getSystemService(SENSOR_SERVICE);
        accelSensor = sm.getDefaultSensor(Sensor.TYPE_LINEAR_ACCELERATION);
        gyroSensor = sm.getDefaultSensor(Sensor.TYPE_GYROSCOPE);


        adapter_delay = new ArrayAdapter<String>(this, android.R.layout.simple_spinner_item, items);
        spinner_delay.setAdapter(adapter_delay);
        spinner_delay.setSelection(2);
        spinner_delay.setOnItemSelectedListener(this);

        btn_reset.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                edit_position.setText("");
                edit_motion.setText("");
                edit_sequence.setText("");
                spinner_delay.setSelection(2);
                delay = SensorManager.SENSOR_DELAY_NORMAL;
                Toast.makeText(MainActivity.this, "설정 초기화", Toast.LENGTH_SHORT).show();
            }
        });

        btn_start.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if (btn_set) {
                    sm.unregisterListener(accelListener);
                    sm.unregisterListener(gyroListener);

                    resetList();
                    btn_reset.setEnabled(false);

                    sm.registerListener(accelListener, accelSensor, delay);
                    sm.registerListener(gyroListener, gyroSensor, delay);
                    starttime = System.currentTimeMillis();

                    Toast.makeText(MainActivity.this, "센서 측정 시작", Toast.LENGTH_SHORT).show();

                    btn_set = false;
                    btn_start.setText("STOP!");
                } else {
                    sm.unregisterListener(accelListener);
                    sm.unregisterListener(gyroListener);
                    endtime = System.currentTimeMillis();
                    btn_start.setEnabled(false);

                    mergeList();
                    saveFile();

                    btn_reset.setEnabled(true);
                    btn_start.setEnabled(true);

                    btn_set = true;
                    btn_start.setText("START!");
                }
            }
        });
    }

    private void mergeList() {
        Store accelTemp;
        Store gyroTemp;
        Iterator accitr = accelList.iterator();
        Iterator gyroitr = gyroList.iterator();

        while (accitr.hasNext() && gyroitr.hasNext()) {
            accelTemp = (Store) accitr.next();
            gyroTemp = (Store) gyroitr.next();
            list.add(new Storage(accelTemp, gyroTemp));
        }
    }

    private void resetList() {
        list.clear();
        accelList.clear();
        gyroList.clear();
    }

    private void saveFile() {
        File path = Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_DOWNLOADS);
        String filepath = "wearable";
        File file = new File(path, filepath);

        Storage storeTemp;
        Iterator itr = list.iterator();
        String title = "accel_x,accel_y,accel_z,accel_ts,gyro_x,gyro_y,gyro_z,gyro_ts\n";
        String time;
        String info = "";

        file.mkdirs();

        String tempfile = edit_motion.getText().toString() + "_" + edit_position.getText().toString()
                + "_" + edit_sequence.getText().toString() + ".csv";

        String filename = file.getPath().toString() + "/" + tempfile;

        Toast.makeText(this, filename + " 파일 저장 중", Toast.LENGTH_SHORT).show();

        try {
            FileOutputStream fos = new FileOutputStream(filename);
            fos.write(title.getBytes());

            while (itr.hasNext()) {
                storeTemp = (Storage) itr.next();
                fos.write(storeTemp.toString().getBytes());
            }
            fos.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
        Toast.makeText(this, "파일 저장 완료", Toast.LENGTH_SHORT).show();
    }


    protected void onResume() {
        super.onResume();
        sm.unregisterListener(accelListener);
        sm.unregisterListener(gyroListener);

        Toast.makeText(MainActivity.this, "재실행 요청", Toast.LENGTH_SHORT).show();
    }

    protected void onPause() {
        super.onPause();
        sm.unregisterListener(accelListener);
        sm.unregisterListener(gyroListener);

        Toast.makeText(MainActivity.this, "센서 측정 정지", Toast.LENGTH_SHORT).show();
    }

    @Override
    protected void onStop() {
        super.onStop();
        sm.unregisterListener(accelListener);
        sm.unregisterListener(gyroListener);

        Toast.makeText(MainActivity.this, "센서 측정 정지", Toast.LENGTH_SHORT).show();
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        sm.unregisterListener(accelListener);
        sm.unregisterListener(gyroListener);

        Toast.makeText(MainActivity.this, "센서 측정 프로그램 종료", Toast.LENGTH_SHORT).show();
    }

    @Override
    public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
        switch (position) {
            case 0:
                delay = SensorManager.SENSOR_DELAY_FASTEST;
                break;
            case 1:
                delay = SensorManager.SENSOR_DELAY_GAME;
                break;
            case 2:
                delay = SensorManager.SENSOR_DELAY_NORMAL;
                break;
            case 3:
                delay = SensorManager.SENSOR_DELAY_UI;
                break;
            default:
                break;
        }
    }

    @Override
    public void onNothingSelected(AdapterView<?> parent) {
    }

    private class AccelListener implements SensorEventListener {
        @Override
        public void onSensorChanged(SensorEvent event) {
            accelList.add(new Store(event.values[0], event.values[1], event.values[2], event.timestamp));
        }

        @Override
        public void onAccuracyChanged(Sensor sensor, int accuracy) {
        }
    }

    private class GyroListener implements SensorEventListener {
        @Override
        public void onSensorChanged(SensorEvent event) {
            gyroList.add(new Store(event.values[0], event.values[1], event.values[2], event.timestamp));
        }

        @Override
        public void onAccuracyChanged(Sensor sensor, int accuracy) {
        }
    }
}