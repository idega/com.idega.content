/*
 * SessionBean1.java
 *
 * Created on 8. okt?ber 2004, 16:14
 * Copyright Roar
 */
package documentmanagementprototype2;

import javax.faces.*;


public class SessionBean1 extends AbstractSessionBean {
    // <editor-fold defaultstate="collapsed" desc="Creator-managed Component Definition">

    // </editor-fold>
    public SessionBean1() {
        // <editor-fold defaultstate="collapsed" desc="Creator-managed Component Initialization">
        try {
        } catch (Exception e) {
            log("SessionBean1 Initialization Failure", e);
            throw e instanceof javax.faces.FacesException ? (FacesException) e : new FacesException(e);
        }
        // </editor-fold>
        // Additional user provided initialization code
    }

    protected documentmanagementprototype2.WebDavTreeBean getWebDavTreeBean() {
        return (documentmanagementprototype2.WebDavTreeBean)getBean("WebDavTreeBean");
    }


    /** 
     * Bean cleanup.
     */
    protected void afterRenderResponse() {
    }
}
