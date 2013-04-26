!set_psf_iso_control.f90
!      module set_psf_iso_control
!
!     Written by H. Matsui on May., 2006
!
!      subroutine set_psf_control(num_mat, mat_name,                    &
!     &          num_surf, surf_name, num_nod_phys, phys_nod_name)
!      subroutine set_iso_control(num_mat, mat_name,                    &
!     &          num_nod_phys, phys_nod_name)
!
      module set_psf_iso_control
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
      private :: read_control_4_psf, read_control_4_iso
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_psf_control(num_mat, mat_name,                     &
     &          num_surf, surf_name, num_nod_phys, phys_nod_name)
!
      use m_control_data_sections
      use m_control_data_4_psf
      use m_control_params_4_psf
      use m_read_control_elements
!
      use set_control_each_psf
      use set_field_comp_for_viz
!
      integer(kind = kint), intent(in) :: num_mat
      character(len=kchara), intent(in) :: mat_name(num_mat)
!
      integer(kind = kint), intent(in) :: num_surf
      character(len=kchara), intent(in) :: surf_name(num_surf)
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      integer(kind = kint) :: i_psf
!
!
      ctl_file_code = psf_ctl_file_code
!
      call allocate_control_params_4_psf
      call allocate_psf_ctl_stract
!
      do i_psf = 1, num_psf
        call read_control_4_psf(i_psf)
      end do
!
      do i_psf = 1, num_psf
        call count_control_4_psf(i_psf, psf_ctl_struct(i_psf),          &
     &      num_mat, mat_name, num_nod_phys, phys_nod_name)
      end do
!
      call allocate_output_comps_4_psf
!
      do i_psf = 1, num_psf
        call set_control_4_psf                                          &
     &     (i_psf, psf_ctl_struct(i_psf), num_mat, mat_name,            &
     &      num_surf, surf_name, num_nod_phys, phys_nod_name)
        call deallocate_cont_dat_4_psf(psf_ctl_struct(i_psf))
      end do
!
      call deallocate_psf_file_header_ctl
!
      call count_total_comps_4_viz(num_psf, num_psf_total_out,          &
     &    istack_psf_output, ncomp_psf_output, max_ncomp_psf_out,       &
     &    num_psf_out_comp)
!
      end subroutine set_psf_control
!
!   --------------------------------------------------------------------
!
      subroutine set_iso_control(num_mat, mat_name,                     &
     &          num_nod_phys, phys_nod_name)
!
      use m_control_data_sections
      use m_control_data_4_iso
      use m_control_params_4_iso
      use m_read_control_elements
!
      use set_control_each_iso
      use set_field_comp_for_viz
!
      integer(kind = kint), intent(in) :: num_mat
      character(len=kchara), intent(in) :: mat_name(num_mat)
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      integer(kind = kint) :: i
!
!
      ctl_file_code = iso_ctl_file_code
!
      call allocate_control_params_4_iso
      call allocate_iso_ctl_stract
!
      do i = 1, num_iso
        call read_control_4_iso(i)
      end do
!
      do i = 1, num_iso
        call count_control_4_iso(i, iso_ctl_struct(i),                  &
     &      num_mat, mat_name, num_nod_phys, phys_nod_name)
      end do
!
      call allocate_output_comps_4_iso
!
      do i = 1, num_iso
        call set_control_4_iso(i, iso_ctl_struct(i),                    &
     &      num_mat, mat_name, num_nod_phys, phys_nod_name)
        call deallocate_cont_dat_4_iso(iso_ctl_struct(i))
      end do
!
      call deallocate_iso_file_header_ctl
!
      call count_total_comps_4_viz(num_iso, num_iso_total_out,          &
     &    istack_iso_output, ncomp_iso_output,  max_ncomp_iso_out,      &
     &   num_iso_out_comp)
!
      if(iflag_debug .gt. 0) then
        do i = 1, num_iso
          write(*,*) 'id_isosurf_data', i,                              &
     &        id_isosurf_data(i), id_isosurf_comp(i)
        end do
      end if
!
      end subroutine set_iso_control
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_control_4_psf(i_psf)
!
      use m_read_control_elements
!
      use m_control_data_4_psf
      use m_control_params_4_psf
      use m_control_data_sections
!
!
      integer(kind = kint), intent(in) :: i_psf
!
!
      if(fname_psf_ctl(i_psf) .eq. 'NO_FILE') return
!
      open(psf_ctl_file_code, file=fname_psf_ctl(i_psf), status='old')
      call read_control_data_4_psf(psf_ctl_struct(i_psf))
      close(psf_ctl_file_code)
!
      end subroutine read_control_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_4_iso(i_iso)
!
      use m_read_control_elements
!
      use m_control_data_4_iso
      use m_control_params_4_iso
      use m_control_data_sections
!
      integer(kind = kint), intent(in) :: i_iso
!
!
      if(fname_iso_ctl(i_iso) .eq. 'NO_FILE') return
!
      open(iso_ctl_file_code, file=fname_iso_ctl(i_iso), status='old')
      call read_control_data_4_iso(iso_ctl_struct(i_iso))
      close(iso_ctl_file_code)
!
      end subroutine read_control_4_iso
!
!  ---------------------------------------------------------------------
!
      end module set_psf_iso_control
