#!/usr/bin/env python
# coding: utf-8

# In[ ]:


import pandas
import math


# In[19]:


data = pandas.read_clipboard(header=None)


# In[20]:


print(data)


# In[23]:


data.columns = ['V1']


# In[35]:


data.V2 = data.V1.apply(lambda x: math.floor(x/3)-2)


# In[36]:


print(data)


# In[37]:


data.V2.sum()


# In[ ]:




