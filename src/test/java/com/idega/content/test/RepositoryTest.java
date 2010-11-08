package com.idega.content.test;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;

import javax.jcr.Credentials;
import javax.jcr.ItemExistsException;
import javax.jcr.LoginException;
import javax.jcr.Node;
import javax.jcr.PathNotFoundException;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.SimpleCredentials;
import javax.jcr.lock.LockException;
import javax.jcr.nodetype.ConstraintViolationException;
import javax.jcr.nodetype.NoSuchNodeTypeException;
import javax.jcr.version.VersionException;

import junit.textui.TestRunner;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.idega.core.content.IdegaRepository;
import com.idega.core.test.base.IdegaBaseTest;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
public class RepositoryTest extends IdegaBaseTest{
	
	private IdegaRepository repository;
	
	private Session session;
	private Credentials credentials = new SimpleCredentials("root","".toCharArray());

	@Override
	@Before
	public void setUp() throws Exception {
		createNewSession();
	}

	private void createNewSession() throws LoginException, RepositoryException {
		session = repository == null ? null : repository.login(credentials);
	}
	
	private Session getSession(){
		return session;
	}
	
	@Test
	public void testContentRepository() {
		//	JCR repository is moved to ePlatform 5.x
		assertEquals(true, true);
	}
	
	public void testRepositoryNodeCreate() throws RepositoryException{
		Node filesNode = getSession().getRootNode().getNode("files");
		Node testNode;
		try{
			testNode = filesNode.getNode("testNode");
		}
		catch(PathNotFoundException pe){
			testNode = filesNode.addNode("testNode");
			testNode.save();
			getSession().save();
		}
		assertNotNull(testNode);
	}
	
	public void testRepositoryFolderCreate() throws RepositoryException{
		Node testFolder = createOrFindTestFolder();
		assertNotNull(testFolder);
	}

	private Node createOrFindTestFolder() throws PathNotFoundException,
			RepositoryException, ItemExistsException, NoSuchNodeTypeException,
			LockException, VersionException, ConstraintViolationException {
		Node filesNode = getSession().getRootNode().getNode("files");
		Node testFolder;
		try{
			testFolder = filesNode.getNode("testFolder");
			testFolder.save();
		}
		catch(PathNotFoundException pe){
			testFolder = filesNode.addNode("testFolder","nt:folder");
		}
		return testFolder;
	}
	
	public void testRepositoryFolderDelete() throws RepositoryException{
		Node testFolder=createOrFindTestFolder();
		testFolder.remove();
		//getSession().save();
		Node filesNode = getSession().getRootNode().getNode("files");
		boolean deleted=false;
		try{
			testFolder = filesNode.getNode("testFolder");
		}
		catch(PathNotFoundException pe){
			//testFolder = filesNode.addNode("testFolder","nt:folder");
			deleted=true;
		}
		assertTrue(deleted);
	}
	
	public void testRepositoryFileCreate() throws PathNotFoundException, ItemExistsException, NoSuchNodeTypeException, LockException, VersionException, ConstraintViolationException, RepositoryException, FileNotFoundException{
		repositoryFileCreateOrUpdate(false);
	}
	
	public void testRepositoryFileUpdate() throws PathNotFoundException, ItemExistsException, NoSuchNodeTypeException, LockException, VersionException, ConstraintViolationException, RepositoryException, FileNotFoundException{
		repositoryFileCreateOrUpdate(true);
	}
	
	public void testRepositoryFileUpdateWithNewSession() throws PathNotFoundException, ItemExistsException, NoSuchNodeTypeException, LockException, VersionException, ConstraintViolationException, RepositoryException, FileNotFoundException{
		createNewSession();
		repositoryFileCreateOrUpdate(true);
	}
	
	public void repositoryFileCreateOrUpdate(boolean alreadyExists) throws PathNotFoundException, ItemExistsException, NoSuchNodeTypeException, LockException, VersionException, ConstraintViolationException, RepositoryException, FileNotFoundException{
		Node testFolder=createOrFindTestFolder();
		Node fileNode;
		Node contentNode;
		String fileTestPath = "src/test/resources/testfile.pdf";
		try{
			
			try{
				//fileNode = rootNode.getNode("/files/cms/themes/Multi_FreeStyle_III/Multi_FreeStyle_III.rwtheme/Contents/css/font/font1.css");
				fileNode = testFolder.getNode("testfile.pdf");
				if(alreadyExists){
					assertEquals(alreadyExists, true);
				}
				System.out.println("Node="+fileNode.getName()+" found");
				String nodeType = fileNode.getPrimaryNodeType().getName();
				if(nodeType.equals("nt:file")){
					System.out.println("NodeType of "+fileNode.getName()+" is "+nodeType);
				}
				else{
					System.err.println("Error: NodeType of "+fileNode.getName()+" is "+nodeType);
				}
			}
			catch(PathNotFoundException ne){
				if(alreadyExists){
					assertEquals(alreadyExists, false);
				}
				fileNode = testFolder.addNode("testfile.pdf", "nt:file");
				System.out.println("Node="+fileNode.getName()+" not found - created");
				//fileNode.save();
			}
			
			
			try{
				//fileNode = rootNode.getNode("/files/cms/themes/Multi_FreeStyle_III/Multi_FreeStyle_III.rwtheme/Contents/css/font/font1.css");
				contentNode = fileNode.getNode("jcr:content");
				System.out.println("Node="+contentNode.getName()+" found");
			}
			catch(PathNotFoundException ne){
				contentNode = fileNode.addNode("jcr:content","nt:unstructured");
				System.out.println("Node="+contentNode.getName()+" not found - created");
			}
			
			FileInputStream fileinstream = new FileInputStream(new File(fileTestPath));
			contentNode.setProperty("jcr:data",fileinstream);
			contentNode.setProperty("jcr:mimetype","application/pdf");
			contentNode.save();
			assertNotNull(contentNode);
		}
		finally{}
		
	}
	
	public static void main(String[] args) throws Exception{
		//SpringJUnit4ClassRunner classRunner = new SpringJUnit4ClassRunner(RepositoryTest.class);
		//classRunner.run(notifier)
		TestRunner.run(new RepositoryTest());
	}
}

