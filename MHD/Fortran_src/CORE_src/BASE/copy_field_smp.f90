!
!      module copy_field_smp
!
!      Written by H. Matsui on June, 2005
!
!> @brief subroutine for data copy
!
!      subroutine copy_nod_scalar_smp(np_smp, numnod, inod_smp_stack,   &
!     &          scalar, copied)
!      subroutine copy_nod_vector_smp(np_smp, numnod, inod_smp_stack,   &
!     &          vector, copied)
!      subroutine copy_nod_sym_tensor_smp(np_smp, numnod,               &
!     &          inod_smp_stack, tensor, copied)
!
!      subroutine copy_nod_integer_smp(np_smp, numnod, inod_smp_stack,  &
!     &          int_scalar, int_copied)
!
      module copy_field_smp
!
      use m_precision
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine copy_nod_scalar_smp(np_smp, numnod, inod_smp_stack,    &
     &          scalar, copied)
!
       integer (kind = kint) :: np_smp, numnod
       integer (kind = kint) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in)    :: scalar(numnod)
       real(kind=kreal), intent(inout) :: copied(numnod)
!
       integer (kind = kint) :: ip, ist, ied, inod
!
!$omp parallel do private(ist,ied)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist,ied
           copied(inod) = scalar(inod)
         end do
      end do
!$omp end parallel do
!
      end subroutine copy_nod_scalar_smp
!
! -----------------------------------------------------------------------
!
      subroutine copy_nod_vector_smp(np_smp, numnod, inod_smp_stack,    &
     &          vector, copied)
!
       integer (kind = kint) :: np_smp, numnod
       integer (kind = kint) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in)    :: vector(numnod,3)
       real(kind=kreal), intent(inout) :: copied(numnod,3)
!
       integer (kind = kint) :: ip, ist, ied, inod
!
!$omp parallel do private(ist,ied,inod)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist,ied
           copied(inod,1) = vector(inod,1)
           copied(inod,2) = vector(inod,2)
           copied(inod,3) = vector(inod,3)
         end do
      end do
!$omp end parallel do
!
      end subroutine copy_nod_vector_smp
!
! -----------------------------------------------------------------------
!
      subroutine copy_nod_sym_tensor_smp(np_smp, numnod,                &
     &          inod_smp_stack, tensor, copied)
!
       integer (kind = kint) :: np_smp, numnod
       integer (kind = kint) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in)    :: tensor(numnod,6)
       real(kind=kreal), intent(inout) :: copied(numnod,6)
!
       integer (kind = kint) :: ip, ist, ied, inod
!
!$omp parallel do private(ist,ied,inod)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist,ied
           copied(inod,1) = tensor(inod,1)
           copied(inod,2) = tensor(inod,2)
           copied(inod,3) = tensor(inod,3)
           copied(inod,4) = tensor(inod,4)
           copied(inod,5) = tensor(inod,5)
           copied(inod,6) = tensor(inod,6)
         end do
      end do
!$omp end parallel do
!
      end subroutine copy_nod_sym_tensor_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_nod_integer_smp(np_smp, numnod, inod_smp_stack,   &
     &          int_scalar, int_copied)
!
       integer (kind = kint) :: np_smp, numnod
       integer (kind = kint) :: inod_smp_stack(0:np_smp)
       integer (kind = kint), intent(in)    :: int_scalar(numnod)
       integer (kind = kint), intent(inout) :: int_copied(numnod)
!
       integer (kind = kint) :: ip, ist, ied, inod
!
!$omp parallel do private(ist,ied)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist,ied
           int_copied(inod) = int_scalar(inod)
         end do
      end do
!$omp end parallel do
!
      end subroutine copy_nod_integer_smp
!
! -----------------------------------------------------------------------
!
      end module copy_field_smp
