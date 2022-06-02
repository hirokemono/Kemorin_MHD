!>@file   cal_sol_reftemp_BiCGSTAB.f90
!!@brief  module cal_sol_reftemp_BiCGSTAB
!!
!!@author H. Matsui
!!@date Programmed in May., 2013
!
!>@brief structures of radial matrix
!!
!!@verbatim
!!      subroutine s_cal_sol_reftemp_BiCGSTAB(band_s00, reftemp)
!!        type(band_matrix_type), intent(in) :: band_s00
!!        real(kind = kreal), intent(inout) :: reftemp(0:smat%n_vect)
!!@endverbatim
!
      module cal_sol_reftemp_BiCGSTAB
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_sph_matrix
      use t_crs_matrix
      use t_crs_connect
!
      implicit none
!
      real(kind = kreal), parameter, private :: eps = 1.0d-10
      real(kind = kreal), parameter, private :: sigma = 1.0d0
      real(kind = kreal), parameter, private :: sigma_diag = 1.0d0
      integer(kind = kint), parameter, private :: itr = 5000
      integer(kind = kint), private :: imonitor_solve = 1
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_cal_sol_reftemp_BiCGSTAB(band_s00, reftemp)
!
      use solver_single
!
      type(band_matrix_type), intent(in) :: band_s00
      real(kind = kreal), intent(inout) ::reftemp(0:band_s00%n_vect)

      type(CRS_matrix_connect) :: tbl3_crs
      type(CRS_matrix) :: mat3_crs
      real(kind = kreal) :: vec_mat(band_s00%n_vect+1)
      real(kind = kreal) :: x_sol(band_s00%n_vect+1)
!
      integer(kind = kint) :: i
!
!
      mat3_crs%PRESET_crs = 1
      mat3_crs%METHOD_crs = 'BiCGSTAB'
      mat3_crs%PRECOND_crs = 'SSOR'
      mat3_crs%INTARRAY_crs(1) =  itr
      mat3_crs%REALARRAY_crs(1) = eps
      mat3_crs%REALARRAY_crs(2) = sigma_diag
      mat3_crs%REALARRAY_crs(3) = sigma
!
      call alloc_crs_stack((band_s00%n_vect+1), tbl3_crs)
!
      tbl3_crs%nitem_l(1) = 0
      tbl3_crs%nitem_u(1) = 1
      do i = 2, band_s00%n_vect
        tbl3_crs%nitem_l(i) = 1
        tbl3_crs%nitem_u(i) = 1
      end do
      tbl3_crs%nitem_l(band_s00%n_vect+1) = 0
      tbl3_crs%nitem_u(band_s00%n_vect+1) = 1
!
      tbl3_crs%istack_l(0) = 0
      tbl3_crs%istack_u(0) = 0
      do i = 1, band_s00%n_vect+1
        tbl3_crs%nitem_l(i) = tbl3_crs%nitem_l(i) + tbl3_crs%nitem_l(i)
        tbl3_crs%nitem_u(i) = tbl3_crs%nitem_u(i) + tbl3_crs%nitem_u(i)
      end do
!
      tbl3_crs%ntot_l = tbl3_crs%nitem_l(band_s00%n_vect+1)
      tbl3_crs%ntot_u = tbl3_crs%nitem_u(band_s00%n_vect+1)
      call alloc_crs_connect(tbl3_crs)
!
      do i = 1, band_s00%n_vect
        tbl3_crs%item_u(i) = i + 1
        tbl3_crs%item_l(i) = i
      end do
!
      mat3_crs%NB_crs = 1
      call alloc_crs_mat_data(tbl3_crs, mat3_crs)
      do i = 1, band_s00%n_vect+1
        mat3_crs%D_crs(i) =  band_s00%mat(2,i-1)
      end do
      do i = 1, band_s00%n_vect
        mat3_crs%AU_crs(i) = band_s00%mat(1,i  )
      end do
      do i = 1, band_s00%n_vect
        mat3_crs%AL_crs(i) = band_s00%mat(3,i-1)
      end do
!
      vec_mat(1:band_s00%n_vect+1) = reftemp(0:band_s00%n_vect)
      call solve_single(tbl3_crs%ntot_d, tbl3_crs%ntot_d,               &
     &    tbl3_crs%ntot_l, tbl3_crs%ntot_u, mat3_crs%D_crs,             &
     &    mat3_crs%AL_crs, tbl3_crs%istack_l, tbl3_crs%item_l,          &
     &    mat3_crs%AU_crs, tbl3_crs%istack_u, tbl3_crs%item_u,          &
     &    vec_mat, x_sol, mat3_crs%PRESET_crs, izero,                   &
     &    mat3_crs%ITERactual, imonitor_solve,                          &
     &    mat3_crs%METHOD_crs, mat3_crs%PRECOND_crs,                    &
     &    mat3_crs%INTARRAY_crs, mat3_crs%REALARRAY_crs)
!
      reftemp(0:band_s00%n_vect) = x_sol(1:band_s00%n_vect+1)
!
      end subroutine s_cal_sol_reftemp_BiCGSTAB
!
! -----------------------------------------------------------------------
!
       end module cal_sol_reftemp_BiCGSTAB
