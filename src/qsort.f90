MODULE Qsort_Module
 
IMPLICIT NONE
 
CONTAINS
 
RECURSIVE SUBROUTINE Qsort(a,b)
 
  REAL, INTENT(IN OUT) :: a(:)
  INTEGER, INTENT(IN OUT) :: b(:)
  INTEGER :: split
 
  IF(size(a) > 1) THEN
     CALL Partition(a, b, split)
     CALL Qsort(a(:split-1),b(:split-1))
     CALL Qsort(a(split:),b(split:))
  END IF
 
END SUBROUTINE Qsort
 
SUBROUTINE Partition(a, b, marker)
 
  REAL, INTENT(IN OUT) :: a(:)
  INTEGER, INTENT(IN OUT) :: b(:)
  INTEGER, INTENT(OUT) :: marker
  REAL :: temp, pivot
  INTEGER :: left, right, temp2
 
  pivot = (a(1) + a(size(a))) / 2  ! Average of first and last elements to prevent quadratic 
  left = 0                         ! behavior with sorted or reverse sorted data
  right = size(a) + 1
 
  DO WHILE (left < right)
     right = right - 1
     DO WHILE (a(right) > pivot)
        right = right-1
     END DO
     left = left + 1
     DO WHILE (a(left) < pivot)
        left = left + 1
     END DO
     IF (left < right) THEN 
        temp = a(left)
        temp2 = b(left)
        a(left) = a(right)
        b(left) = b(right)
        a(right) = temp
        b(right) = temp2
     END IF
  END DO
 
  IF (left == right) THEN
     marker = left + 1
  ELSE
     marker = left
  END IF
 
END SUBROUTINE Partition
 
END MODULE Qsort_Module
