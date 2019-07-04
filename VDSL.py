import signal

def terminacion():
  answ = conn.response
  conn.close()
  print answ
  
signal.signal(signal.SIGALRM, terminacion) 

signal.alarm(3) 

import hashlib
import Exscript
from Exscript.protocols import SSH2
from Exscript import Account
account = Account('tecnico','PASSWORD')
conn = SSH2()
conn.connect('IP')
conn.login(account)
conn.execute('enable')
try:
  conn.execute('display vdsl line operation board 0/2')
  #
except:
  try:
    conn.execute('\n')
    
  except:
    conn.execute('display vdsl line operation board 0/2')
  

answ = conn.response
conn.close()

print answ






# import pexpect

