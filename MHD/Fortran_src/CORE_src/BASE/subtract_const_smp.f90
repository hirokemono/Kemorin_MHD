!
!      module subtract_const_smp
!
!      Written by H. Matsui on June, 2005
!
!      subroutine subtruct_const_4_scalar_smp(np_smp, numnod,           &
!     &          inod_smp_stack, dest, scalar, const)
!      subroutine subtruct_const_4_vect_smp(np_smp, numnod,             &
!     &          inod_smp_stack, dest, vector, const)
!      subroutine subtruct_const_4_tensor_smp(np_smp, numnod,           &
!     &          inod_smp_stack, dest, tensor, const)
!
!      subroutine subtruct_const_4_scalar_smp_ow(np_smp, numnod,        &
!     &          inod_smp_stack, scalar, const)
!      subroutine subtruct_const_4_vect_smp_ow(np_smp, numnod,          &
!     &          inod_smp_stack, vector, const)
!      subroutine subtruct_const_4_tensor_smp_ow(np_smp, numnod,        &
!     &          inod_smp_stack, tensor, const)
!
      module subtract_const_smp
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
      subroutine subtruct_const_4_scalar_smp(np_smp, numnod,            &
     &          inod_smp_stack, dest, scalar, const)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in)    :: const
       real(kind=kreal), intent(in) :: scalar(numnod)
!
       real(kind=kreal), intent(inout) :: dest(numnod)
!
       integer (kind = kint) :: ip, ist, ied, inod
!
!$omp parallel do private(ist,ied,inod)
!cdir parallel do private(ist,ied,inod)
!poption parallel
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist,ied
           dest(inod) = scalar(inod) - const
         end do
       end do
!$omp end parallel do
!
      end subroutine subtruct_const_4_scalar_smp
!
! -----------------------------------------------------------------------
!
      subroutine subtruct_const_4_vect_smp(np_smp, numnod,              &
     &          inod_smp_stack, dest, vector, const)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in)    :: const
       real(kind=kreal), intent(in) :: vector(numnod,3)
!
       real(kind=kreal), intent(inout) :: dest(numnod,3)
!
       integer (kind = kint) :: ip, ist, ied, inod
!
!$omp parallel do private(ist,ied,inod)
!cdir parallel do private(ist,ied,inod)
!poption parallel
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist,ied
           dest(inod,1) = vector(inod,1) - const
           dest(inod,2) = vector(inod,2) - const
           dest(inod,3) = vector(inod,3) - const
         end do
       end do
!$omp end parallel do
!
      end subroutine subtruct_const_4_vect_smp
!
! -----------------------------------------------------------------------
!
      subroutine subtruct_const_4_tensor_smp(np_smp, numnod,            &
     &          inod_smp_stack, dest, tensor, const)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in)    :: const
       real(kind=kreal), intent(in) :: tensor(numnod,6)
!
       real(kind=kreal), intent(inout) :: dest(numnod,6)
!
       integer (kind = kint) :: ip, ist, ied, inod
!
!$omp parallel do private(ist,ied,inod)
!cdir parallel do private(ist,ied,inod)
!poption parallel
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist,ied
           dest(inod,1) = tensor(inod,1) - const
           dest(inod,2) = tensor(inod,2) - const
           dest(inod,3) = tensor(inod,3) - const
           dest(inod,4) = tensor(inod,4) - const
           dest(inod,5) = tensor(inod,5) - const
           dest(inod,6) = tensor(inod,6) - const
         end do
       end do
!$omp end parallel do
!
      end subroutine subtruct_const_4_tensor_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine subtruct_const_4_scalar_smp_ow(np_smp, numnod,         &
     &          inod_smp_stack, scalar, const)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in)    :: const
!
       real(kind=kreal), intent(inout) :: scalar(numnod)
!
       integer (kind = kint) :: ip, ist, ied, inod
!
!$omp parallel do private(ist,ied,inod)
!cdir parallel do private(ist,ied,inod)
!poption parallel
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist,ied
           scalar(inod) = scalar(inod) - const
         end do
       end do
!$omp end parallel do
!
      end subroutine subtruct_const_4_scalar_smp_ow
!
! -----------------------------------------------------------------------
!
      subroutine subtruct_const_4_vect_smp_ow(np_smp, numnod,           &
     &          inod_smp_stack, vector, const)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in)    :: const
!
       real(kind=kreal), intent(inout) :: vector(numnod,3)
!
       integer (kind = kint) :: ip, ist, ied, inod
!
!$omp parallel do private(ist,ied,inod)
!cdir parallel do private(ist,ied,inod)
!poption parallel
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist,ied
           vector(inod,1) = vector(inod,1) - const
           vector(inod,2) = vector(inod,2) - const
           vector(inod,3) = vector(inod,3) - const
         end do
       end do
!$omp end parallel do
!
      end subroutine subtruct_const_4_vect_smp_ow
!
! -----------------------------------------------------------------------
!
      subroutine subtruct_const_4_tensor_smp_ow(np_smp, numnod,         &
     &          inod_smp_stack, tensor, const)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in)    :: const
!
       real(kind=kreal), intent(inout) :: tensor(numnod,6)
!
       integer (kind = kint) :: ip, ist, ied, inod
!
!$omp parallel do private(ist,ied,inod)
!cdir parallel do private(ist,ied,inod)
!poption parallel
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist,ied
           tensor(inod,1) = tensor(inod,1) - const
           tensor(inod,2) = tensor(inod,2) - const
           tensor(inod,3) = tensor(inod,3) - const
           tensor(inod,4) = tensor(inod,4) - const
           tensor(inod,5) = tensor(inod,5) - const
           tensor(inod,6) = tensor(inod,6) - const
         end do
      end do
!$omp end parallel do
!
      end subroutine subtruct_const_4_tensor_smp_ow
!
! -----------------------------------------------------------------------
!
      end module subtract_const_smp
