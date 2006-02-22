/*
 * WebDavTreeBean.java
 *
 * Created on 22. oktÃ¯Â¿Â½ber 2004, 14:57
 */

package documentmanagementprototype2;

import javax.faces.*;
import java.io.Serializable;

import javax.faces.context.FacesContext;
import javax.faces.event.ActionListener;
import org.apache.commons.httpclient.HttpURL;
import org.apache.commons.httpclient.HttpException;
import org.apache.webdav.lib.WebdavResource;
import org.apache.webdav.lib.WebdavException;
import java.io.IOException;



/**
 * @author Roar
 */
public class WebDavTreeBean extends AbstractSessionBean implements Serializable, ActionListener {
    // <editor-fold defaultstate="collapsed" desc="Creator-managed Component Definition">

    // </editor-fold>
    public WebDavTreeBean(){
    // <editor-fold defaultstate="collapsed" desc="Creator-managed Component Initialization">
    try {
    } catch (Exception e) {
        log("WebDavTreeBean Initialization Failure", e);
        throw e instanceof FacesException ? (FacesException) e : new FacesException(e);
    }
    // </editor-fold>
    // Additional user provided initialization code
}

//    protected documentmanagementprototype2.ApplicationBean1 getApplicationBean1() {
//        return (documentmanagementprototype2.ApplicationBean1)getBean("ApplicationBean1");
//    }

    protected documentmanagementprototype2.SessionBean1 getSessionBean1() {
        return (documentmanagementprototype2.SessionBean1)getBean("SessionBean1");
    }

    /** 
     * Bean cleanup.
     */
    protected void afterRenderResponse() {
    }

    //private WebTree tree;


    private String davHost = "localhost";
    private String davPort = "8080";
    WebdavResource cloneResource(WebdavResource resource) throws WebdavException, HttpException, IOException {
        HttpURL url = new HttpURL("http://"+davHost+":"+davPort+resource.getPath());
        url.setUserinfo("root","root");
        WebdavResource newResource = new WebdavResource(url);
        if (! newResource.exists()){
            throw new WebdavException("Resource not found.");
        }

        return newResource;
    } 

   /* public void setTree(WebTree theTree) {
        if (tree == null){
        	theTree.setExpandedIcon("../images/owtfolderexp.gif");
        	theTree.setIcon("../images/owtfoldercxp.gif");
        	theTree.setMinusIcon("../images/owtminusxp.gif");
        	theTree.setPlusIcon("../images/owtplusxp.gif");
            initTree(theTree);
            TreeNode rootNode = (TreeNode) theTree.getChildren().get(0); 
            
            
//            rootNode.hide();
            
        } else {
            //refresh the tree, starting from root
            ((SlideTreeNode) tree.getChildren().get(0)).refresh();
        }
        tree = theTree;
    }*/

    /*private void initTree(WebTree tree) {
        FacesContext fContext = javax.faces.context.FacesContext.getCurrentInstance();
        try {
            tree.getChildren().add(getSubTree(getRoot(), fContext));
//            test(getRoot());
        } catch (Exception ex){
            tree.getChildren().add(new TreeNode("Exception: " + ex.getMessage() + " " + ex.getClass().getName()));
        }
    }*/
   


    public void setTreeAction(){
        System.out.println("setTreeAction");
    }

    documentmanagementprototype2.WebDavTreeBean.SlideTreeNode getSubTree(WebdavResource resource, FacesContext fContext) throws IOException, HttpException{
        documentmanagementprototype2.WebDavTreeBean.SlideTreeNode node = new documentmanagementprototype2.WebDavTreeBean.SlideTreeNode(resource);
        //node.setActionListener(getApplication().createMethodBinding("#{WebDavTree.processAction}", new Class[] {javax.faces.event.ActionEvent.class }));
        //node.setImmediate(true);
        //node.setId(fContext.getViewRoot().createUniqueId());

        WebdavResource[] resources = resource.getChildResources().listResources();
        for (int i =0; i<resources.length; i++){
            if (resources[i].isCollection()){
                node.addTreeNode(getSubTree(cloneResource(resources[i]), fContext));
            }
        }
        return node;
    }
    private String _selectedCollectionUri;
    public void processAction(javax.faces.event.ActionEvent actionEvent) throws javax.faces.event.AbortProcessingException {
            /*documentmanagementprototype2.WebDavTreeBean.SlideTreeNode node = (SlideTreeNode) actionEvent.getComponent();
            WebdavResource resource = node.getWebdavResource();
            try{
                _selectedCollectionUri = resource.getHttpURL().getURI();
                java.lang.System.out.print("...");
            }catch (org.apache.commons.httpclient.URIException ex){
                java.lang.System.out.println(ex.getMessage());

            }*/
            java.lang.System.out.print("Action event received");

    }
    
    public String getUri(){
        return _selectedCollectionUri;
    }
    /*
    private TreeNode getNode(TreeNode tree, String id){
        TreeNode foundNode;
        java.util.Iterator children = tree.getChildren().iterator();
        while (children.hasNext()){
            TreeNode child = (TreeNode) children.next();
            if (child.getId().equals(id)){
                return child;
            } else {
                return getNode(child, id);
            }
        }
    }*/


        class SlideTreeNode {//extends TreeNode {
            private boolean _refreshed = true;
            private java.util.Map _children = new java.util.HashMap();
            private WebdavResource _resource;
            
            private SlideTreeNode(){}
            public SlideTreeNode(WebdavResource resource){
                //super(resource.getDisplayName());
                _resource = resource;
            }
            public WebdavResource getWebdavResource(){
                return _resource;
            }
            public boolean exists(){
                return _resource.exists();
            }
            public void addTreeNode(SlideTreeNode child){
                //super.getChildren().add(child);
                _children.put(child.getDisplayName(), child);
            }
            public String getDisplayName(){
                return _resource.getDisplayName();
            }
            public SlideTreeNode findChild(String displayName){
                return (SlideTreeNode) _children.get(displayName);
            }
            public boolean hasChild(String displayname){
                return findChild(displayname) != null;
            }
            public boolean isRefreshed(){
                return _refreshed ;
            }
            public void setDirty(){
                _refreshed = false;
            }        
            
            public void refresh() {
                //mark all children as dirty (not refreshed)
                java.util.Iterator childNodes = _children.values().iterator();
                while(childNodes.hasNext()){
                    ((SlideTreeNode) childNodes.next()).setDirty();
                }
                
                //adding new nodes and mark existing as refreshed
                try{ 
                    WebdavResource[] childResources = _resource.getChildResources().listResources();

                    for (int i =0; i<childResources.length; i++){
                        if (childResources[i].isCollection()){            
                            if (! hasChild(childResources[i].getDisplayName())){ 
                                addTreeNode(getSubTree(cloneResource(childResources[i]), javax.faces.context.FacesContext.getCurrentInstance()));

                            } else {
                                findChild(childResources[i].getDisplayName()).refresh();
                            }
                        }
                    }
        
                }catch(Exception ex){ 
                       ex.printStackTrace();
                }                
                
                //remove still dirty (not refreshed) nodes
                childNodes = _children.values().iterator();
                while(childNodes.hasNext()){
                    SlideTreeNode childNode = (SlideTreeNode) childNodes.next();
                    if (! childNode.isRefreshed()){
                        
                    	// getChildren().remove(childNode);
                        
                    }
                }
                
                _refreshed = true;
            }


            
            public void hide(){
                 
            }

            /*
            public void encodeBegin(FacesContext fContext) throws IOException{
    	    if(_visible){
                    super.encodeBegin(fContext);
                }else{
                    super.encodeChildren(fContext);
                }
            }   

       
            public void encodeEnd(FacesContext fContext) throws IOException{
                super.encodeEnd(fContext);
            }
    */
            
       
        }    

}
