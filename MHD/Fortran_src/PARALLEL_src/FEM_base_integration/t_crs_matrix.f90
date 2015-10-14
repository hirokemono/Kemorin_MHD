!
!     module t_crs_matrix
!
!      Written by Kemorin
!
!      subroutine alloc_crs_matrix(numnod, tbl_crs, mat_crs)
!      subroutine alloc_crs_mat_data(numnod, tbl_crs, mat_crs)
!
!      subroutine dealloc_crs_mat_data(mat_crs)
!      subroutine check_crs_matrix_comps(my_rank, tbl_crs, mat_crs)
!
      module t_crs_matrix
!
      use m_precision
      use t_crs_connect
!
      implicit none
!
      type CRS_matrix
        integer(kind=kint ) ::  NB_crs = 3
!>  Total size of non-0 component of matrix
        integer (kind = kint) :: ntot_A
!>  CRS matrix
        real(kind=kreal), pointer ::  A_crs(:)
!>  Diagonal component
        real(kind=kreal), pointer ::  D_crs(:)
!>  lower left component
        real(kind=kreal), pointer ::  AL_crs(:)
!>  upper right component
        real(kind=kreal), pointer ::  AU_crs(:)

        real(kind=kreal), pointer ::  B_crs(:)
        real(kind=kreal), pointer ::  X_crs(:)
!
!>   pointer for diagonal component
        integer (kind = kint) :: ist_d
!>   pointer for upper part of matrix
        integer (kind = kint) :: ist_u
!>   pointer for lower part of matrix
        integer (kind = kint) :: ist_l

        character(len=kchara) :: SOLVER_crs
        character(len=kchara) :: PRECOND_crs
        character(len=kchara) :: METHOD_crs

        real(kind = kreal)  ::  REALARRAY_crs(10) = 0.0d0
        integer(kind = kint) :: INTARRAY_crs(10) = 0
        integer(kind=kint )                  ::  errno
!
        integer(kind=kint) ::  PRESET_crs = 2
        integer(kind=kint) ::  ITERactual
      end type CRS_matrix
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_crs_matrix(numnod, tbl_crs, mat_crs)
!
      integer(kind = kint), intent(in) :: numnod
      type(CRS_matrix_connect), intent(inout) :: tbl_crs
      type(CRS_matrix), intent(inout) :: mat_crs
!
!
      call alloc_crs_stack(numnod, tbl_crs)
      call alloc_crs_connect(tbl_crs)
!
      call alloc_crs_mat_data(tbl_crs, mat_crs)
!
      end subroutine alloc_crs_matrix
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_crs_mat_data(tbl_crs, mat_crs)
!
      type(CRS_matrix_connect), intent(in) :: tbl_crs
      type(CRS_matrix), intent(inout) :: mat_crs
!
      integer(kind = kint) :: NB
!
!
      NB = mat_crs%NB_crs
      mat_crs%ntot_A = tbl_crs%ntot_d + tbl_crs%ntot_l + tbl_crs%ntot_u
      mat_crs%ist_d = 1
      mat_crs%ist_l = 1 + NB*NB *  tbl_crs%ntot_d
      mat_crs%ist_u = 1 + NB*NB * (tbl_crs%ntot_d + tbl_crs%ntot_l)
!
      allocate(mat_crs%A_crs(1-NB*NB:NB*NB*mat_crs%ntot_A))
      if(mat_crs%ntot_A .gt. 0) mat_crs%A_crs = 0.0d0
!
      mat_crs%D_crs =>  mat_crs%A_crs(1:mat_crs%ist_l-1)
      mat_crs%AL_crs => mat_crs%A_crs(mat_crs%ist_l:mat_crs%ist_u-1)
      mat_crs%AU_crs => mat_crs%A_crs(mat_crs%ist_u:mat_crs%ntot_A)
!
      allocate(mat_crs%B_crs(NB*tbl_crs%ntot_d))
      allocate(mat_crs%X_crs(NB*tbl_crs%ntot_d))
!
      if(tbl_crs%ntot_d .gt. 0) mat_crs%B_crs = 0.0d0
      if(tbl_crs%ntot_d .gt. 0) mat_crs%X_crs = 0.0d0
!
      end subroutine alloc_crs_mat_data
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_crs_mat_data(mat_crs)
!
      type(CRS_matrix), intent(inout) :: mat_crs
!
      nullify (mat_crs%D_crs, mat_crs%AL_crs, mat_crs%AU_crs)
      deallocate (mat_crs%A_crs)
      deallocate (mat_crs%B_crs, mat_crs%X_crs)
!
      end subroutine dealloc_crs_mat_data
!
!  ---------------------------------------------------------------------
!
      subroutine check_crs_matrix_comps(my_rank, tbl_crs, mat_crs)
!
      integer(kind = kint), intent(in) :: my_rank
      type(CRS_matrix_connect), intent(in) :: tbl_crs
      type(CRS_matrix), intent(inout) :: mat_crs
!
       integer (kind = kint) :: i, k1, k2, j, NB, kst, ked
!
       NB = mat_crs%NB_crs
       do i = 1, tbl_crs%ntot_d
           write(my_rank+50,*) "vector (inod) = ", i
           write(my_rank+50,'(1p5e16.8)')                               &
     &           (mat_crs%B_crs(NB*(i-1)+k2),k2=1,NB)
       end do
!
       do i = 1, tbl_crs%ntot_d
         do k1 = 1, NB
           kst = k1 + (i-1) * NB*NB
           ked = k1 + (NB-1) * NB + (i-1) * NB*NB
           write(my_rank+50,*) "diagonal (inod,k1) = ", i, k1
           write(my_rank+50,'(1p5e16.8)')                               &
     &                  mat_crs%D_crs(kst:ked:NB)
         end do
       end do
!
       do i = 1, tbl_crs%ntot_d
         do j = tbl_crs%istack_l(i-1)+1, tbl_crs%istack_l(i)
           do k1 = 1, NB
             kst = k1 + (j-1) * NB*NB
             ked = k1 + (NB-1) * NB + (j-1) * NB*NB
             write(my_rank+50,*) "Lower component (i1,i2,k1) = ",      &
     &             i, tbl_crs%item_l(j), k1
             write(my_rank+50,'(1p5e16.8)')                            &
     &                  mat_crs%AL_crs(kst:ked:NB)
           end do
         end do
       end do
!
       do i = 1, tbl_crs%ntot_d
         do j = tbl_crs%istack_u(i-1)+1, tbl_crs%istack_u(i)
           do k1 = 1, NB
             kst = k1 + (j-1) * NB*NB
             ked = k1 + (NB-1) * NB + (j-1) * NB*NB
             write(my_rank+50,*) "Lower component (i1,i2,k1) = ",      &
     &             i, tbl_crs%item_u(j), k1
              write(my_rank+50,'(1p5e16.8)')                           &
     &                  mat_crs%AU_crs(kst:ked:NB)
           end do
         end do
       end do
!
       end subroutine check_crs_matrix_comps
!
!  ---------------------------------------------------------------------
!
      end module t_crs_matrix
