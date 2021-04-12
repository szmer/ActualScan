package com.actualscan.crawling;

import java.util.Collection;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.List;

import org.apache.storm.task.IOutputCollector;
import org.apache.storm.tuple.Tuple;

/* MockOutputCollector lets us test behavior of bolts without running a topology. */
public class MockOutputCollector implements IOutputCollector {

   public HashMap<String, ArrayList<List<Object>>> emittedTuples =
      new HashMap<String, ArrayList<List<Object>>>();

   public List<Integer> emit(String streamId, Collection<Tuple> anchors,
         List<Object> tuple) {
      if (! emittedTuples.containsKey(streamId)) {
         emittedTuples.put(streamId, new ArrayList<List<Object>>());
      }
      ArrayList<List<Object>> streamTuples = emittedTuples.get(streamId);
      streamTuples.add(tuple);
      emittedTuples.put(streamId, streamTuples);
      return new ArrayList<Integer>();
   }

   public void emitDirect(int taskId, String streamId, Collection<Tuple> anchors,
         List<Object> tuple) {
      if (! emittedTuples.containsKey(streamId)) {
         emittedTuples.put(streamId, new ArrayList<List<Object>>());
      }
      ArrayList<List<Object>> streamTuples = emittedTuples.get(streamId);
      streamTuples.add(tuple);
      emittedTuples.put(streamId, streamTuples);
   }
   
   public void ack(Tuple t) {
      return;
   }
   
   public void fail(Tuple t) {
      return;
   }

   public void resetTimeout(Tuple t) {
      return;
   }

   public void reportError(Throwable error) {
      return;
   }
}
