/*
 * Copyright 2005 The Apache Software Foundation.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.idega.content.tree;

import org.apache.myfaces.custom.tree2.TreeNodeBase;

/**
 * Convenience implementation of <code>TreeNode</code> to make the tree
 * easy to use with boolean checkboxes.
 *
 * @author Matthias Wessendorf (changed by $Author: justinas $)
 * @version $Revision: 1.1 $ $Date: 2006/09/22 12:35:00 $
 */
public class TreeNodeChecked extends TreeNodeBase
{
	private static final long serialVersionUID = -3319932828983347196L;
	private boolean checked;

	public TreeNodeChecked() {
		super();
	}

	public TreeNodeChecked(String type, String description, boolean leaf) {
		super(type, description, leaf);
	}

	public TreeNodeChecked(String type, String description, String identifier, boolean leaf) {
		super(type, description, identifier, leaf);
	}
	
	public TreeNodeChecked(String type, String description, boolean checked, boolean leaf) {
		super(type, description, leaf);
		this.checked = checked;
	}
	
	public TreeNodeChecked(String type, String description, String identifier, boolean checked, boolean leaf) {
		super(type, description, identifier, leaf);
		this.checked = checked;
	}


	public boolean isChecked() {
		return checked;
	}

	public void setChecked(boolean checked) {
		this.checked = checked;
	}

}