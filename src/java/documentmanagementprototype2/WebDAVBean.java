/*
 * WebDAVBean.java
 *
 * Created on 15. okt?ber 2004, 13:11
 */

package documentmanagementprototype2;

import java.beans.*;
import java.io.Serializable;

/**
 * @author Roar
 */
public class WebDAVBean extends Object implements Serializable {
    
    public static final String PROP_ID = "id";
    public static final String PROP_NAME = "name";
    public static final String PROP_LENGTH = "length";
    public static final String PROP_IS_COLLECTION = "is_collection";
    public static final String PROP_DATE = "date";
    public static final String PROP_MIME = "mime";
    
    private int id;
    private String name;
    private long length;
    private boolean isCollection;
    private String date;
    private String mime;
    
    private PropertyChangeSupport propertySupport;
    
    public WebDAVBean() {
        propertySupport = new PropertyChangeSupport(this);
    }
   
    public WebDAVBean(String name) {
        this();
        setName(name);
    }      
               
    public WebDAVBean(String name, boolean isCollection, long length, long date, String mime) {
        this(name);
       setIsCollection(isCollection);
       setLength(length);
       setDate(new java.util.Date(date).toString());
       setMime(mime);
       setId((int)Math.round(Math.random())*1000);
    }    
    
    public int getId() {
        return id;
    }
    
    public void setId(int value) {
        int oldValue = id;
        id = value;
        propertySupport.firePropertyChange(PROP_ID, oldValue, id);
    }
    
   
    public long getLength() {
        return length;
    }
    
    public void setLength(long value) {
        long oldValue = length;
        length = value;
//        propertySupport.firePropertyChange(PROP_LENGTH, oldValue, length);
    }
    
    public String getName() {
        return name;
    }
    
    public void setName(String value) {
        String oldValue = name;
        name = value;
        propertySupport.firePropertyChange(PROP_NAME, oldValue, name);
    }
    
    public String getDate() {
        return date;
    }
    
    public void setDate(String value) {
        String oldValue = date;
        date = value;
        propertySupport.firePropertyChange(PROP_DATE, oldValue, date);
    }
    
    public String getMime() {
        return mime;
    }
    
    public void setMime(String value) {
        String oldValue = mime;
        mime = value;
        propertySupport.firePropertyChange(PROP_MIME, oldValue, mime);
    }
    
    public boolean getIsCollection() {
        return isCollection;
    }
    
    public void setIsCollection(boolean value) {
        boolean oldValue = isCollection;
        isCollection = value; 
        propertySupport.firePropertyChange(PROP_IS_COLLECTION, oldValue, isCollection); 
    }
    
    
    public void addPropertyChangeListener(PropertyChangeListener listener) {
        propertySupport.addPropertyChangeListener(listener);
    }
    
    public void removePropertyChangeListener(PropertyChangeListener listener) {
        propertySupport.removePropertyChangeListener(listener);
    }
    
}
