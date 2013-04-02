!mag_of_field_smp.f90
!      module mag_of_field_smp
!
!      Written by H. Matsui on June, 2005
!
!      subroutine cal_vector_magnitude(np_smp, numnod, inod_smp_stack,  &
!     &          mag, vect)
!      subroutine cal_sym_tensor_magnitude(np_smp, numnod,              &
!     &          inod_smp_stack, mag, vect)
!      subroutine cal_asym_tensor_magnitude(np_smp, numnod,             &
!     &          inod_smp_stack, mag, vect)
!
      module mag_of_field_smp
!
      use m_precision
      use m_constants
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_vector_magnitude(np_smp, numnod, inod_smp_stack,   &
     &          mag, vect)
!
       integer (kind = kint) :: np_smp, numnod
       integer (kind = kint) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in)    :: vect(numnod,3)
       real(kind=kreal), intent(inout) :: mag(numnod)
!
       integer (kind = kint) :: inod, ip, ist, ied
!
!$omp parallel do private(ist,ied,inod)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
!
           mag(inod) = sqrt( vect(inod,1)*vect(inod,1)                  &
     &                     + vect(inod,2)*vect(inod,2)                  &
     &                     + vect(inod,3)*vect(inod,3) )
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_vector_magnitude
!
! -----------------------------------------------------------------------
!
      subroutine cal_sym_tensor_magnitude(np_smp, numnod,               &
     &          inod_smp_stack, mag, vect)
!
       integer (kind = kint) :: np_smp, numnod
       integer (kind = kint) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in)    :: vect(numnod,6)
       real(kind=kreal), intent(inout) :: mag(numnod)
!
       integer (kind = kint) :: inod, ip, ist, ied
!
!$omp parallel do private(ist,ied,inod)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
!
           mag(inod) = sqrt(     vect(inod,1)*vect(inod,1)              &
     &                     + two*vect(inod,2)*vect(inod,2)              &
     &                     + two*vect(inod,3)*vect(inod,3)              &
     &                     +     vect(inod,4)*vect(inod,4)              &
     &                     + two*vect(inod,5)*vect(inod,5)              &
     &                     +     vect(inod,6)*vect(inod,6) )
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_sym_tensor_magnitude
!
! -----------------------------------------------------------------------
!
      subroutine cal_asym_tensor_magnitude(np_smp, numnod,              &
     &          inod_smp_stack, mag, vect)
!
       integer (kind = kint) :: np_smp, numnod
       integer (kind = kint) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in)    :: vect(numnod,3)
       real(kind=kreal), intent(inout) :: mag(numnod)
!
       integer (kind = kint) :: inod, ip, ist, ied
!
!$omp parallel do private(ist,ied,inod)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
!
           mag(inod) = two*sqrt( vect(inod,1)*vect(inod,1)              &
     &                         + vect(inod,2)*vect(inod,2)              &
     &                         + vect(inod,3)*vect(inod,3) )
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_asym_tensor_magnitude
!
! -----------------------------------------------------------------------
!
      end module mag_of_field_smp
