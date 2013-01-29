pro calc_permutation
  last = 6

  for i=1,last do print,i
    
  for i=1,last do $
    for j=i+1, last do print, i, j
    
  for i=1,last do $
    for j=i+1,last do $
      for k=j+1,last do print, i, j, k
    
  for i=1,last do $
    for j=i+1,last do $
      for k=j+1,last do $
        for l=k+1, last do print, i, j, k, l
    
  for i=1,last do $
    for j=i+1,last do $
      for k=j+1,last do $
        for l=k+1, last do $
          for m=l+1,last do print, i, j, k, l, m
    
  for i=1,last do $
    for j=i+1,last do $
      for k=j+1,last do $
        for l=k+1, last do $
          for m=l+1,last do $
            for n=m+1,last do print, i, j, k, l, m, n
    
end

