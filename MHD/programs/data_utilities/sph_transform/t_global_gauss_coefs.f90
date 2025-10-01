!>@file   t_global_gauss_coefs.f90
!!@brief  module t_global_gauss_coefs
!!
!!@author H. Matsui
!!@date Programmed in March, 2012
!
!>@brief Gauss coefficients data for output
!!
!!@verbatim
!!      subroutine alloc_gauss_global_coefs(d_gauss)
!!      subroutine dealloc_gauss_global_coefs(d_gauss)
!!        type(global_gauss_points), intent(inout) :: d_gauss
!!
!!      subroutine read_gauss_global_coefs(d_gauss, iend)
!!      subroutine write_gauss_global_coefs(d_gauss)
!!        type(global_gauss_points), intent(inout) :: d_gauss
!!
!!      subroutine set_ctl_4_global_gauss_coefs(fhead_ctl, d_gauss)
!!        type(read_character_item), intent(in) :: gauss_sph_fhead_ctl
!!        type(global_gauss_points), intent(inout) :: d_gauss
!!
!!      subroutine set_poloidal_b_by_gauss_coefs                        &
!!     &         (sph_params, sph_rj, ipol, d_gauss, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(phys_address), intent(in) :: ipol
!!        type(global_gauss_points), intent(in) :: d_gauss
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
!
      module t_global_gauss_coefs
!
      use m_precision
!
      implicit none
!
      integer(kind = kint), parameter :: id_gauss = 12
!
      type global_gauss_points
        integer(kind = kint) :: ltr_w
        integer(kind = kint) :: jmax_w
        integer(kind = kint), allocatable :: index_w(:,:)
        real(kind = kreal), allocatable :: w_gauss(:)
        real(kind = kreal) :: r_gauss
!
        character(len = kchara) :: fhead_gauss
      end type global_gauss_points
!
      private :: id_gauss
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine alloc_gauss_global_coefs(d_gauss)
!
      type(global_gauss_points), intent(inout) :: d_gauss
!
      integer(kind = kint) :: j, l, m
!
!
      d_gauss%jmax_w = d_gauss%ltr_w*(d_gauss%ltr_w+2)
!
      allocate(d_gauss%index_w(d_gauss%jmax_w,2))
      allocate(d_gauss%w_gauss(d_gauss%jmax_w))
!
      d_gauss%w_gauss(1:d_gauss%jmax_w) = 0.0d0
      do l = 1, d_gauss%ltr_w
        do m = -l, l
          j = l*(l + 1) + m
          d_gauss%index_w(j,1) = l
          d_gauss%index_w(j,2) = m
        end do
      end do
!
      end subroutine alloc_gauss_global_coefs
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine dealloc_gauss_global_coefs(d_gauss)
!
      type(global_gauss_points), intent(inout) :: d_gauss
!
!
      deallocate(d_gauss%index_w, d_gauss%w_gauss)
!
      end subroutine dealloc_gauss_global_coefs
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine read_gauss_global_coefs(i_step, d_gauss, iend)
!
      use skip_comment_f
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: i_step
      type(global_gauss_points), intent(inout) :: d_gauss
      integer(kind = kint), intent(inout) :: iend
!
      character(len=kchara) :: fname_gauss, fname_tmp
      character(len=255) :: character_4_read
      integer(kind = kint) :: l, m, j
      real(kind = kreal) :: rtmp
!
!
      fname_tmp = add_int_suffix(i_step, d_gauss%fhead_gauss)
      fname_gauss = add_dat_extension(fname_tmp)
      open(id_gauss,file = fname_gauss)
!
      call skip_comment(id_gauss, character_4_read, iend)
      if(iend .gt. 0) go to 99
      read(character_4_read,*) d_gauss%ltr_w, d_gauss%r_gauss
!
      call alloc_gauss_global_coefs(d_gauss)
!
      call skip_comment(id_gauss, character_4_read, iend)
      if(iend .gt. 0) go to 99
      read(character_4_read,*) l, m, rtmp
      j = l*(l+1) + m
      if(j .le. d_gauss%jmax_w) d_gauss%w_gauss(j) = rtmp
!
      do
        read(id_gauss,*,err=99,end=99) l, m, rtmp
        j = l*(l+1) + m
        if(j .le. d_gauss%jmax_w) d_gauss%w_gauss(j) = rtmp
      end do
!
  99  continue
      close(id_gauss)
      if(iend .gt. 0) write(*,*)                                        &
     &              'Read file error in read_gauss_global_coefs'
!
      write(*,*) 'j, index_w(j,1:2), w_gauss(j)'
      do j = 1, d_gauss%jmax_w
        write(*,*) j, d_gauss%index_w(j,1:2), d_gauss%w_gauss(j)
      end do
!
      end subroutine read_gauss_global_coefs
!
!  -------------------------------------------------------------------
!
      subroutine write_gauss_global_coefs(i_step, d_gauss)
!
      use skip_comment_f
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: i_step
      type(global_gauss_points), intent(in) :: d_gauss
!
      character(len=kchara) :: fname_gauss, fname_tmp
!
!
      fname_tmp = add_int_suffix(i_step, d_gauss%fhead_gauss)
      fname_gauss = add_dat_extension(fname_tmp)
      open(id_gauss,file = fname_gauss)
!
      write(id_gauss,'(a)') '#'
      write(id_gauss,'(a)') '# truncation, radius for potential'
      write(id_gauss,'(a)') '#'
!
      write(id_gauss,'(i16,1pe25.15e3)') d_gauss%ltr_w, d_gauss%r_gauss
!
!
      write(id_gauss,'(a)') '#'
      write(id_gauss,'(a)') '# degree, order, potential'
      write(id_gauss,'(a)') '#'
!
      close(id_gauss)
!
      end subroutine write_gauss_global_coefs
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine set_ctl_4_global_gauss_coefs(fhead_ctl, d_gauss)
!
      use t_control_array_character
!
      type(read_character_item), intent(in) :: fhead_ctl
      type(global_gauss_points), intent(inout) :: d_gauss
!
!
      if(fhead_ctl%iflag .gt. 0) then
        d_gauss%fhead_gauss = fhead_ctl%charavalue
      end if
!
      end subroutine set_ctl_4_global_gauss_coefs
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine set_poloidal_b_by_gauss_coefs                          &
     &         (sph_params, sph_rj, ipol, d_gauss, rj_fld)
!
      use t_spheric_parameter
      use t_spheric_rj_data
      use t_phys_address
      use t_phys_data
!
      use extend_potential_field
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol
      type(global_gauss_points), intent(in) :: d_gauss
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      write(*,*) ' ipol%base%i_magne', ipol%base%i_magne,               &
     &          sph_params%nlayer_ICB, sph_params%nlayer_CMB
      if (ipol%base%i_magne .gt. 0) then
        call gauss_to_poloidal_out                                      &
     &     (sph_params%nlayer_CMB, d_gauss%ltr_w, d_gauss%r_gauss,      &
     &      d_gauss%w_gauss, d_gauss%index_w, sph_rj, rj_fld%n_point,   &
     &      rj_fld%d_fld(1,ipol%base%i_magne))
        call gauss_to_poloidal_in                                       &
     &     (sph_params%nlayer_ICB, d_gauss%ltr_w, d_gauss%r_gauss,      &
     &      d_gauss%w_gauss, d_gauss%index_w, sph_rj, rj_fld%n_point,   &
     &      rj_fld%d_fld(1,ipol%base%i_magne))
      end if
!
      end subroutine set_poloidal_b_by_gauss_coefs
!
! -----------------------------------------------------------------------
!
      end module t_global_gauss_coefs
