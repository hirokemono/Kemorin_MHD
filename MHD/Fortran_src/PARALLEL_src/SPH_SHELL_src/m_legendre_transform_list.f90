!>@file   m_legendre_transform_list.f90
!!@brief  module m_legendre_transform_list
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  Legendre transform selector
!!
!!
!!@verbatim
!!      subroutine set_legendre_trans_mode_ctl(tranx_loop_ctl)
!!      subroutine display_selected_legendre_mode(id_legendre)
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module m_legendre_transform_list
!
      use m_precision
!
      implicit none
!
!>      Number of LEgendre transform types
      integer(kind = kint), parameter :: maxindex_Leg_trans_loop = 17
!
!>      Character flag to perform Legendre transform 
!@n     using original array order
      character(len = kchara), parameter                                &
     &           :: leg_orginal_loop = 'original_loop'
!>      Character flag to perform Legendre transform 
!@n     using blocked loop
      character(len = kchara), parameter                                &
     &           :: leg_blocked_loop = 'blocked_loop'
!>      Character flag to perform Legendre transform 
!!@n    using longer loop for original array order 
      character(len = kchara), parameter                                &
     &           :: leg_krloop_inner = 'inner_radial_loop'
!>      Character flag to perform Legendre transform 
!@n     with inneromst Legendre polynomial loop
      character(len = kchara), parameter                                &
     &           :: leg_krloop_outer = 'outer_radial_loop'
!>      Character flag to perform Legendre transform 
!@n     with symmetry
      character(len = kchara), parameter                                &
     &           :: leg_sym_org_loop =   'symmetric_original_loop'
!>      Character flag to perform Legendre transform 
!@n     with inneromst Legendre polynomial loop
      character(len = kchara), parameter                                &
     &           :: leg_sym_spin_loop = 'symmetric_outer_radial_loop'
!
!>      Character flag to perform Legendre transform 
!@n     with mutmul function
      character(len = kchara), parameter                                &
     &           :: leg_matmul = 'matmul'
!>      Character flag to perform Legendre transform 
!@n     with dgemm in BLAS
      character(len = kchara), parameter                                &
     &           :: leg_dgemm = 'BLAS'
!>      Character flag to perform Legendre transform 
!@n     with self matrix product
      character(len = kchara), parameter                                &
     &           :: leg_matprod = 'matproduct'
!
!>      Chalacter flag to perform Legendre transform 
!@n     with symmetry and mutmul function
      character(len = kchara), parameter                                &
     &           :: leg_sym_matmul =  'symmetric_matmul'
!>      Character flag to perform Legendre transform 
!@n     with symmetry and dgemm in BLAS
      character(len = kchara), parameter                                &
     &           :: leg_sym_dgemm =   'symmetric_BLAS'
!>      Character flag to perform Legendre transform 
!@n     with symmetry and  self matrix product
      character(len = kchara), parameter                                &
     &           :: leg_sym_matprod = 'symmetric_matproduct'
!
!>      Chalacter flag to perform Legendre transform 
!@n     with symmetry and mutmul function
      character(len = kchara), parameter                                &
     &           :: leg_sym_matmul_big =  'symmetric_matmul_big'
!>      Character flag to perform Legendre transform 
!@n     with symmetry and dgemm in BLAS
      character(len = kchara), parameter                                &
     &           :: leg_sym_dgemm_big =   'symmetric_BLAS_big'
!>      Character flag to perform Legendre transform 
!@n     with symmetry and  self matrix product
      character(len = kchara), parameter                                &
     &           :: leg_sym_matprod_big = 'symmetric_matproduct_big'
!
!>      Character flag to perform Legendre transform 
!@n     with testing loop
      character(len = kchara), parameter                                &
     &           :: leg_test_loop =    'test_loop'
!
!>      Chalacter flag to perform Legendre transform 
!@n     with symmetry and mutmul function
      character(len = kchara), parameter                                &
     &           :: leg_sym_mat_jt =  'Pjt_matmul_theta_OMP'
!>      Character flag to perform Legendre transform 
!@n     with symmetry and dgemm in BLAS
      character(len = kchara), parameter                                &
     &           :: leg_sym_dgemm_jt =   'Pjt_BLAS_w_theta_OMP'
!
!>      Chalacter flag to perform Legendre transform 
!@n     with symmetry and mutmul function
      character(len = kchara), parameter                                &
     &           :: leg_sym_mat_tj =  'Ptj_matmul_theta_OMP'
!>      Character flag to perform Legendre transform 
!@n     with symmetry and dgemm in BLAS
      character(len = kchara), parameter                                &
     &           :: leg_dgemm_tj =   'Ptj_BLAS_w_theta_OMP'
!
!>      Character flag to perform Legendre transform 
!@n     with symmetry and on-the-fly Legendre polynomial
      character(len = kchara), parameter                                &
     &           :: on_the_fly_matmul =   'On_the_fly_Plm_matmul'
!>      Character flag to perform Legendre transform 
!@n     with symmetry and on-the-fly Legendre polynomial
      character(len = kchara), parameter                                &
     &           :: on_the_fly_dgemm =    'On_the_fly_Plm_BLAS'
!>      Character flag to perform Legendre transform 
!@n     with symmetry and on-the-fly Legendre polynomial
      character(len = kchara), parameter                                &
     &           :: on_the_fly_matprod =  'On_the_fly_Plm'
!
!>      integer flag to run elpse time check for legendre transform
      integer(kind = kint), parameter :: iflag_leg_undefined =    -1
!
!>      integer flag to perform Legendre transform with symmetry
      integer(kind = kint), parameter :: iflag_leg_symmetry =      1
!>      integer flag to perform Legendre transform 
!@n     with symmetry and inneromst Legendre polynomial loop
      integer(kind = kint), parameter :: iflag_leg_sym_spin_loop = 2
!>      integer flag to perform Legendre transform 
!@n     with symmetry and mutmul function
      integer(kind = kint), parameter :: iflag_leg_sym_matmul =    3
!>      integer flag to perform Legendre transform 
!@n     with symmetry and dgemm in BLAS
      integer(kind = kint), parameter :: iflag_leg_sym_dgemm =    13
!>      integer flag to perform Legendre transform 
!@n     with symmetry and mutmul function
      integer(kind = kint), parameter :: iflag_leg_sym_matmul_big =  4
!>      integer flag to perform Legendre transform 
!@n     with symmetry and dgemm in BLAS
      integer(kind = kint), parameter :: iflag_leg_sym_dgemm_big = 14
!>      integer flag to perform Legendre transform 
!@n     with symmetry and  self matrix product
      integer(kind = kint), parameter :: iflag_leg_sym_mat_jt = 5
!>      integer flag to perform Legendre transform 
!@n     with symmetry and  self matrix product
      integer(kind = kint), parameter :: iflag_leg_sym_dgemm_jt = 15
!>      integer flag to perform Legendre transform 
!@n     with symmetry and  self matrix product
      integer(kind = kint), parameter :: iflag_leg_sym_mat_tj = 6
!>      integer flag to perform Legendre transform 
!@n     with symmetry and  self matrix product
      integer(kind = kint), parameter :: iflag_leg_sym_dgemm_tj = 16
!
!>      integer flag to perform Legendre transform 
!@n     with symmetry and on-the-fly Legendre polynomial
      integer(kind = kint), parameter :: iflag_on_the_fly_matmul = 7
!>      integer flag to perform Legendre transform 
!@n     with symmetry and on-the-fly Legendre polynomial
      integer(kind = kint), parameter :: iflag_on_the_fly_dgemm = 17
!>      integer flag to perform Legendre transform 
!@n     with symmetry and on-the-fly Legendre polynomial
      integer(kind = kint), parameter :: iflag_on_the_fly_matprod = 8
!
!>      integer flag to perform Legendre transform 
!@n     with testing loop
      integer(kind = kint), parameter :: iflag_leg_test_loop =   99
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function set_legendre_trans_mode_ctl         &
     &                            (tranx_loop_ctl)
!
      use skip_comment_f
!
      character(len = kchara), intent(in) :: tranx_loop_ctl
!
!
      if(     cmp_no_case(tranx_loop_ctl, leg_test_loop)) then
        set_legendre_trans_mode_ctl = iflag_leg_test_loop
      else if(cmp_no_case(tranx_loop_ctl, leg_sym_org_loop)) then
        set_legendre_trans_mode_ctl = iflag_leg_symmetry
      else if(cmp_no_case(tranx_loop_ctl, leg_sym_spin_loop)) then
        set_legendre_trans_mode_ctl = iflag_leg_sym_spin_loop
!
      else if(cmp_no_case(tranx_loop_ctl, leg_sym_matmul)) then
        set_legendre_trans_mode_ctl = iflag_leg_sym_matmul
      else if(cmp_no_case(tranx_loop_ctl, leg_sym_dgemm)) then
        set_legendre_trans_mode_ctl = iflag_leg_sym_dgemm
!
      else if(cmp_no_case(tranx_loop_ctl, leg_sym_matmul_big)) then
        set_legendre_trans_mode_ctl = iflag_leg_sym_matmul_big
      else if(cmp_no_case(tranx_loop_ctl, leg_sym_dgemm_big)) then
        set_legendre_trans_mode_ctl = iflag_leg_sym_dgemm_big
!
      else if(cmp_no_case(tranx_loop_ctl, leg_sym_mat_jt)) then
        set_legendre_trans_mode_ctl = iflag_leg_sym_mat_jt
      else if(cmp_no_case(tranx_loop_ctl, leg_sym_dgemm_jt)) then
        set_legendre_trans_mode_ctl = iflag_leg_sym_dgemm_jt
!
      else if(cmp_no_case(tranx_loop_ctl, leg_sym_mat_tj)) then
        set_legendre_trans_mode_ctl = iflag_leg_sym_mat_tj
      else if(cmp_no_case(tranx_loop_ctl, leg_dgemm_tj)) then
        set_legendre_trans_mode_ctl = iflag_leg_sym_dgemm_tj
!
      else if(cmp_no_case(tranx_loop_ctl, on_the_fly_matmul)) then
        set_legendre_trans_mode_ctl = iflag_on_the_fly_matmul
      else if(cmp_no_case(tranx_loop_ctl, on_the_fly_dgemm)) then
        set_legendre_trans_mode_ctl = iflag_on_the_fly_dgemm
      else if(cmp_no_case(tranx_loop_ctl, on_the_fly_matprod)) then
        set_legendre_trans_mode_ctl = iflag_on_the_fly_matprod
!
      else
        set_legendre_trans_mode_ctl = iflag_leg_symmetry
      end if
!
      end function set_legendre_trans_mode_ctl
!
! -----------------------------------------------------------------------
!
      subroutine display_selected_legendre_mode(id_legendre)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_legendre
!
      character(len=kchara) :: tmpchara
!
!
      if(id_legendre .eq. iflag_leg_symmetry) then
        write(tmpchara,'(a)') trim(leg_sym_org_loop)
       else if(id_legendre .eq. iflag_leg_sym_spin_loop) then
        write(tmpchara,'(a)') trim(leg_sym_spin_loop)
!
      else if(id_legendre .eq. iflag_leg_sym_matmul) then
        write(tmpchara,'(a)') trim(leg_sym_matmul)
      else if(id_legendre .eq. iflag_leg_sym_dgemm) then
        write(tmpchara,'(a)') trim(leg_sym_dgemm)
!
      else if(id_legendre .eq. iflag_leg_sym_matmul_big) then
        write(tmpchara,'(a)') trim(leg_sym_matmul_big)
      else if(id_legendre .eq. iflag_leg_sym_dgemm_big) then
        write(tmpchara,'(a)') trim(leg_sym_dgemm_big)
!
      else if(id_legendre .eq. iflag_leg_sym_mat_jt) then
        write(tmpchara,'(a)') trim(leg_sym_mat_jt)
      else if(id_legendre .eq. iflag_leg_sym_dgemm_jt) then
        write(tmpchara,'(a)') trim(leg_sym_dgemm_jt)
!
      else if(id_legendre .eq. iflag_leg_sym_mat_tj) then
        write(tmpchara,'(a)') trim(leg_sym_mat_tj)
      else if(id_legendre .eq. iflag_leg_sym_dgemm_tj) then
        write(tmpchara,'(a)') trim(leg_dgemm_tj)
!
      else if(id_legendre .eq. iflag_on_the_fly_matmul) then
        write(tmpchara,'(a)') trim(on_the_fly_matmul)
      else if(id_legendre .eq. iflag_on_the_fly_dgemm) then
        write(tmpchara,'(a)') trim(on_the_fly_dgemm)
      else if(id_legendre .eq. iflag_on_the_fly_matprod) then
        write(tmpchara,'(a)') trim(on_the_fly_matprod)
!
      else if(id_legendre .eq. iflag_leg_test_loop) then
        write(tmpchara,'(a)') trim(leg_test_loop)
      end if
      call change_2_upper_case(tmpchara)
!
      write(*,'(a,i4)', advance='no')                                 &
     &       'Selected id_legendre_transfer: ', id_legendre
      write(*,'(a,a,a)') ' (', trim(tmpchara), ') '
!
      end subroutine display_selected_legendre_mode
!
! -----------------------------------------------------------------------
!
      end module m_legendre_transform_list
