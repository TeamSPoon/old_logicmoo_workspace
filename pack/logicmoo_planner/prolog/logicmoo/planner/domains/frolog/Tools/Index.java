package Tools;

import java.util.*;

public class Index<K,V> {
    HashMap<K,ArrayList<V>> map;
 
    public Index() {
        map = new HashMap<K,ArrayList<V>>();
    }
    
    public Collection values(){
    	return map.values();
    }
    
    public void put(K key, V value) {
        ArrayList<V> list = map.get(key);
        if ( list == null ) {
            list = new ArrayList<V>();
            map.put(key, list);
        }
        list.add(value);
    }
    public V get(K key, int index) {
        ArrayList<V> list = map.get(key);
        if ( list == null ) {
            return null;
        }
        if ( index >= list.size() || index < 0 ) {
            return null;
        }
        return list.get(index);
    }
    // other possible useful methods would be:
    // boolean remove(K key, V targetValue);
    public V remove(K key, int index) {
        ArrayList<V> list = map.get(key);
        if ( list == null ) {
            return null;
        }
        if ( index >= list.size() || index < 0 ) {
            return null;
        }
        V v = list.remove(index);
        if ( list.size() == 0 ) {
            map.remove(key);
        }
        return v;
    }
 
    // instead of using .size() I split the 
    // functionality into three different
    // methods returning the necessary
    // information. Remember, now the number
    // of keys could be less than the number
    // of values.
    public int getValueCount() {
        int size = 0;
        for ( ArrayList<V> list : map.values()) {
            size += list.size();
        }
        return size;
    }
    public int getValueCount(K key) {
        ArrayList<V> list = map.get(key);
        if ( list == null ) {
            return 0;
        }
        return list.size();
    }
    public int getKeyCount() {
        return map.size();
    }
 
    public boolean containsKey(K key) {
        return map.containsKey(key);
    }
    public boolean containsKey(K key, int index) {
        ArrayList<V> list = map.get(key);
        if ( list == null ) {
            return false;
        }
        if ( index >= list.size() || index < 0 ) {
            return false;
        }
        return true;
    }
    public boolean isEmpty() {
        return map.isEmpty();
    }
 
    // other possible useful methods would be:
    // boolean containsValue(K key, V targetValue);
    public boolean containsValue(V targetValue) {
        for ( ArrayList<V> list : map.values()) {
            for ( V value : list ) {
                if ( targetValue == value )
                    return true;
            }
        }
        return false;
    }
    
    public void clear() {
        map.clear();
    }
}

