/*
 * WebDAVBean.java
 *
 * Created on 15. oktober 2004, 13:11
 */

package documentmanagementprototype2;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import org.apache.webdav.lib.WebdavResource;
import com.idega.core.file.business.FileIconSupplier;

import com.idega.slide.util.VersionHelper;

/**
 * @author Roar
 */
public class WebDAVBean extends Object {
    
    public static final String PROP_ID = "id";
    public static final String PROP_NAME = "name";
    public static final String PROP_LENGTH = "length";
    public static final String PROP_IS_COLLECTION = "is_collection";
    public static final String PROP_MODIFIED_DATE = "mod_date";
    public static final String PROP_CREATION_DATE = "cre_date";
    public static final String PROP_MIME = "mime";
    public static final String PROP_WEB_DAV_URL = "webdav_url";
    public static final String PROP_ICON_URL = "icon_url";
    public static final String PROP_VERSION = "version";
    
    private int id;
    private String name;
    private long length;
    private boolean isCollection;
    private String modifiedDate;
    private String creationDate;
    private String mime;
    private String webDavUrl;
    private String iconURL;
    private String version;
    private PropertyChangeSupport propertySupport;
    
    public WebDAVBean() {
    	
    	propertySupport = new PropertyChangeSupport(this);
	    setId((int)Math.round(Math.random())*1000);
  		
    }
   
    public WebDAVBean(String name) {
        this();
        setName(name);
    }      
   
    public WebDAVBean(WebdavResource resource) {
    	this();
			setName(resource.getDisplayName());
			setIsCollection(resource.isCollection());
			setLength(resource.getGetContentLength()); 
			setModifiedDate(resource.getGetLastModified());
			setMime(resource.getGetContentType());
			setCreationDate(resource.getCreationDate());
			setWebDavHttpURL(resource.getPath());
			setVersion(VersionHelper.getLatestVersion(resource));
    }
    
    public WebDAVBean(String name, boolean isCollection, long length, long modifieDate, String mime) {
        this(name);
       setIsCollection(isCollection);
       setLength(length);
       setModifiedDate(modifieDate);
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
//        long oldValue = length;
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
    
    public String getModifiedDate() {
        return modifiedDate;
    }
    
    public void setModifiedDate(long value) {
    	setModifiedDate(new java.util.Date(value).toString());
    }
    public void setModifiedDate(String value) {
        String oldValue = modifiedDate;
        modifiedDate = value;
        propertySupport.firePropertyChange(PROP_MODIFIED_DATE, oldValue, modifiedDate);
    }
    
    public String getCreationDate() {
      return creationDate;
    }
  
	  public void setCreationDate(long value) {
	  	setCreationDate(new java.util.Date(value).toString());
	  }

	  public void setCreationDate(String value) {
	      String oldValue = creationDate;
	      creationDate = value;
	      propertySupport.firePropertyChange(PROP_CREATION_DATE, oldValue, creationDate);
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
    
    public String getWebDavUrl() {
    		return webDavUrl;
    }
    
    public void setWebDavHttpURL(String webDavUrl) {
	    	String oldValue = this.webDavUrl;
	    	this.webDavUrl = webDavUrl;
	    	propertySupport.firePropertyChange(PROP_WEB_DAV_URL, oldValue, webDavUrl);
    }    
    
    public String getVersion() {
    	return version;
    }
    
    public void setVersion(String version) {
    	String oldVersion = this.version;
    	this.version = version;
    	propertySupport.firePropertyChange(PROP_VERSION, oldVersion, version);
    }
    
    public void addPropertyChangeListener(PropertyChangeListener listener) {
        propertySupport.addPropertyChangeListener(listener);
    }
    
    public void removePropertyChangeListener(PropertyChangeListener listener) {
        propertySupport.removePropertyChangeListener(listener);
    }
    
	public String getIconURL() {
		if(iconURL==null){
			FileIconSupplier iconSupplier = FileIconSupplier.getInstance();
			iconURL = iconSupplier.getFileIconURIByMimeType(mime);
		}
		return iconURL;
	}
	
	
	public void setIconURL(String iconURL) {
	  	String oldValue = this.iconURL;
	  	this.iconURL = iconURL;
	  	propertySupport.firePropertyChange(PROP_ICON_URL, oldValue, iconURL);
	}
}
