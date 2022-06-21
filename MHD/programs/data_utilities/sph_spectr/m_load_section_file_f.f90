!>@file   m_load_section_file_f.f90
!!@brief  module m_load_section_file_f
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief Top subroutines to read sectioning data
!!
!!@verbatim
!!      subroutine check_gauss_coef_series_f(cname)                     &
!!     &          bind(c, name="check_gauss_coef_series_f")
!!        character(1,C_char), intent(in) :: cname(*)
!!      integer(c_int) function                                         &
!!    &     load_gauss_coefs_series_f(cname, cstart, cend) Bind(C)
!!        character(1,C_char), intent(in) :: cname(*)
!!        real(C_double), Value :: cstart, cend
!!
!!      subroutine get_gauss_coefs_time_f(n_step, i_step, time)         &
!!     &          bind(c, name="get_gauss_coefs_time_f")
!!        integer(C_int), Value :: n_step
!!        integer(C_int), intent(inout) :: i_step(n_step)
!!        real(c_double), intent(inout) :: time(n_step)
!!      subroutine get_each_gauss_coef_series_f(yname, n_step, d_pick)  &
!!     &          bind(c, name="get_each_gauss_coef_series_f")
!!        character(1,C_char), intent(in) :: yname(*)
!!        integer(C_int), Value :: n_step
!!        real(c_double), intent(inout) :: d_pick(n_step)
!!      subroutine fin_gauss_coefs_series_f()                           &
!!     &          bind(c, name="fin_gauss_coefs_series_f")
!!        integer(C_int), Value :: n_step
!!@endverbatim
!
      module m_load_section_file_f
!
      use ISO_C_BINDING
!
      use m_precision
      use m_constants
!
      use t_psf_results
      use t_time_data
      use t_ucd_data
      use t_file_IO_parameter
!
      implicit  none
!
      type(time_data), save, private :: time_data_p
      type(psf_results), save, private :: psf_data_p
      type(ucd_data), save, private :: psf_ucd
!
      type(field_IO_params), save, private :: psf_file_param
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      integer(C_int) function check_section_file_f(cname)               &
     &              bind(c, name="check_section_file_f")
!
      use count_monitor_time_series
      use set_ucd_file_names
!
      character(1,C_char), intent(in) :: cname(*)
      character(len=kchara) :: file_name
!
      integer(kind = kint) :: istep_viz
!
!
      write(file_name,'(a)') trim(c_to_fstring(cname))
      call viz_file_format_from_file_name(file_name,                    &
     &    psf_file_param%iflag_format, psf_file_param%file_prefix,      &
     &    istep_viz)
      write(*,*) 'input_file_name: ', trim(file_name)
      write(*,*) 'prefix without step: ',                               &
     &          trim(psf_file_param%file_prefix)
      write(*,*) 'Firmat ID: ', psf_file_param%iflag_format
      write(*,*) 'Read step: ', istep_viz
!
      check_section_file_f = istep_viz
!
      end function check_section_file_f
!
! -------------------------------------------------------------------
!
      integer(C_int) function load_section_file_f                       &
     &             (istep, numnod, numele)                              &
     &              bind(c, name="load_section_file_f")
!
      integer(C_int), Value :: istep
      integer(C_int), intent(inout) :: numnod(1), numele(1)
      integer(kind = kint) :: istep_viz, i
!
      istep_viz = istep
      call load_psf_data_to_link_IO                                     &
     &   (istep_viz, psf_file_param, time_data_p, psf_data_p, psf_ucd)
      numnod = psf_data_p%psf_nod%numnod
      numele = psf_data_p%psf_ele%numele
!
      write(*,*) 'Index, # of componentns, field_name'
      do i = 1, psf_data_p%psf_phys%num_phys
        write(*,*) i, psf_data_p%psf_phys%num_component(i),             &
     &                trim(psf_data_p%psf_phys%phys_name(i))
      end do
      load_section_file_f = psf_data_p%psf_phys%num_phys
!
      end function load_section_file_f
!
! -------------------------------------------------------------------
!
      integer(C_int) function finalize_section_data_f()                 &
     &              bind(c, name="finalize_section_data_f")
!
      call disconnect_ucd_mesh(psf_ucd)
      call dealloc_psf_results(psf_data_p)
      finalize_section_data_f = 0
!
      end function finalize_section_data_f
!
! -------------------------------------------------------------------
!
      integer(C_int) function get_xy_section_patch_f                    &
     &             (numnod, numele, x_psf, y_psf, ie_psf)               &
     &              bind(c, name="get_xy_section_patch_f")
!
      use gauss_coefs_monitor_IO
!
      integer(C_int), Value :: numnod, numele
      real(c_double), intent(inout) :: x_psf(numnod), y_psf(numnod)
      integer(c_int), intent(inout) :: ie_psf(3*numele)
!
      integer(kind = kint) :: i
!
!$omp parallel do
      do i = 1, psf_data_p%psf_nod%numnod
        x_psf(i) = psf_data_p%psf_nod%xx(i,1)
        y_psf(i) = psf_data_p%psf_nod%xx(i,2)
      end do
!$omp end parallel do
!
!$omp parallel do
      do i = 1, psf_data_p%psf_ele%numele
        ie_psf(3*i-2) = psf_data_p%psf_ele%ie(i,1) - 1
        ie_psf(3*i-1) = psf_data_p%psf_ele%ie(i,2) - 1
        ie_psf(3*i  ) = psf_data_p%psf_ele%ie(i,3) - 1
      end do
!$omp end parallel do
!
      get_xy_section_patch_f = 0
      end function get_xy_section_patch_f
!
! -------------------------------------------------------------------
!
      integer(C_int) function get_scalar_section_data_f                 &
     &             (numnod, id_field, id_comp, scalar)                  &
     &              bind(c, name="get_scalar_section_data_f")
!
      use gauss_coefs_monitor_IO
!
      integer(C_int), Value :: numnod, id_field, id_comp
      real(c_double), intent(inout) :: scalar(numnod)
!
      integer(kind = kint) :: i, icomp
!
      icomp = id_comp                                                   &
     &       + psf_data_p%psf_phys%istack_component(id_field-1)
!$omp parallel do
      do i = 1, psf_data_p%psf_nod%numnod
        scalar(i) = psf_data_p%psf_phys%d_fld(i,icomp)
      end do
!$omp end parallel do
!
      get_scalar_section_data_f = 0
      end function get_scalar_section_data_f
!
! -------------------------------------------------------------------
!
      end module m_load_section_file_f
