!
!     module m_crs_matrix
!
!      Written by Kemorin
!
!       subroutine allocate_crs_matrix(numnod)
!       subroutine allocate_crs_mat_data(numnod)
!
!       subroutine deallocate_crs_mat_data
!       subroutine check_crs_matrix_components(my_rank, numnod)
!
      module m_crs_matrix
!
      use m_precision
!
      implicit none
!
      integer(kind=kint ) ::  NB_crs = 3

      real   (kind=kreal), allocatable ::  AL_crs(:,:,:)
      real   (kind=kreal), allocatable ::  AU_crs(:,:,:)
      real   (kind=kreal), allocatable ::  D_crs(:,:,:)

      real   (kind=kreal), allocatable ::  B_crs(:)
      real   (kind=kreal), allocatable ::  X_crs(:)

      character(len=kchara) :: SOLVER_crs
      character(len=kchara) :: PRECOND_crs, METHOD_crs

      real(kind = kreal),  dimension(10) :: REALARRAY_crs = 0.0d0
      integer(kind = kint), dimension(10) ::  INTARRAY_crs = 0
      integer(kind=kint )                  ::  errno
!
      integer(kind=kint) ::  PRESET_crs = 2, ITERactual
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
       subroutine allocate_crs_matrix(numnod)
!
       use m_crs_connect
!
       integer(kind = kint), intent(in) :: numnod
!
!
       call allocate_crs_stack(numnod)
       call allocate_crs_connect
!
       call allocate_crs_mat_data(numnod)
!
       end subroutine allocate_crs_matrix
!
!  ---------------------------------------------------------------------
!
       subroutine allocate_crs_mat_data(numnod)
!
       use m_crs_connect
!
       integer(kind = kint), intent(in) :: numnod
!
!
       allocate(AL_crs(NB_crs,NB_crs,ntot_crs_l) )
       allocate(AU_crs(NB_crs,NB_crs,ntot_crs_u) )
       allocate(D_crs(NB_crs,NB_crs,numnod))
       allocate(B_crs(NB_crs*numnod), X_crs(NB_crs*numnod))
!
       AL_crs = 0.0d0
       AU_crs = 0.0d0
       D_crs = 0.0d0
       B_crs = 0.0d0
       X_crs = 0.0d0
!
       end subroutine allocate_crs_mat_data
!
!  ---------------------------------------------------------------------
!
       subroutine deallocate_crs_mat_data
!
       deallocate (AL_crs)
       deallocate (AU_crs)
       deallocate (D_crs)
       deallocate (B_crs, X_crs)
!
       end subroutine deallocate_crs_mat_data
!
!  ---------------------------------------------------------------------
!
       subroutine check_crs_matrix_components(my_rank, numnod)
!
       use m_crs_connect
!
       integer(kind = kint), intent(in) :: my_rank, numnod
       integer (kind = kint) :: i, k1, k2, j
!
       do i = 1, numnod
           write(my_rank+50,*) "vector (inod) = ", i
           write(my_rank+50,'(1p5e16.8)')                               &
     &           (B_crs(NB_crs*(i-1)+k2),k2=1,NB_crs)
       end do
!
       do i = 1, numnod
         do k1 = 1, NB_crs
          write(my_rank+50,*) "diagonal (inod,k1) = ", i, k1
           write(my_rank+50,'(1p5e16.8)') (D_crs(k1,k2,i),k2=1,NB_crs)
         end do
       end do
!
       do i = 1, numnod
         do j = istack_crs_l(i-1)+1, istack_crs_l(i)
           do k1 = 1, NB_crs
             write(my_rank+50,*) "Lower component (i1,i2,k1) = ",      &
     &             i, item_crs_l(j), k1
             write(my_rank+50,'(1p5e16.8)')                            &
     &                   (AL_crs(k1,k2,j),k2=1,NB_crs)
           end do
         end do
       end do
!
       do i = 1, numnod
         do j = istack_crs_u(i-1)+1, istack_crs_u(i)
           do k1 = 1, NB_crs
             write(my_rank+50,*) "Lower component (i1,i2,k1) = ",      &
     &             i, item_crs_u(j), k1
              write(my_rank+50,'(1p5e16.8)')                           &
     &                  (AU_crs(k1,k2,j),k2=1,NB_crs)
           end do
         end do
       end do
!
       end subroutine check_crs_matrix_components
!
!  ---------------------------------------------------------------------
!
      end module m_crs_matrix
