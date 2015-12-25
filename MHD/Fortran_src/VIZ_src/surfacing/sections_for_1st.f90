!sections_for_1st.f90
!      module sections_for_1st
!
!      Written by H. Matsui on Apr., 2012
!
!      subroutine init_visualize_surface(nod_fld)
!      subroutine visualize_surface(istep_psf, istep_iso, nod_fld)
!
!      subroutine cross_section_init_1st(nod_fld)
!      subroutine isosurface_init_1st(nod_fld)
!
!      subroutine cross_section_main_1st(istep_psf, nod_fld)
!      subroutine isosurface_main_1st(istep_iso, nod_fld)
!        type(phys_data), intent(in) :: nod_fld
!
      module sections_for_1st
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_phys_data
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_visualize_surface(nod_fld)
!
      use m_control_data_sections
      use m_cross_section
      use m_isosurface
!
      use set_psf_case_table
!
      type(phys_data), intent(in) :: nod_fld
!
!
      if ( (num_psf_ctl+num_iso_ctl) .gt. 0) then
        if (iflag_debug.eq.1)  write(*,*) 'set_sectioning_case_table'
        call set_sectioning_case_table
      end if
!
      num_psf = num_psf_ctl
      if (num_psf .gt. 0)  call cross_section_init_1st(nod_fld)
!
      num_iso = num_iso_ctl
      if (num_iso .gt. 0) call isosurface_init_1st(nod_fld)
!
      end subroutine init_visualize_surface
!
!  ---------------------------------------------------------------------
!
      subroutine visualize_surface(istep_psf, istep_iso, nod_fld)
!
      use m_cross_section
      use m_isosurface
!
      integer(kind = kint), intent(in) :: istep_psf, istep_iso
      type(phys_data), intent(in) :: nod_fld
!
!
      if (num_psf.gt.0 .and. istep_psf.gt.0) then
        call cross_section_main_1st(istep_psf, nod_fld)
      end if
      if (num_iso.gt.0 .and. istep_iso.gt.0) then
        call isosurface_main_1st(istep_iso, nod_fld)
      end if
!
      end subroutine visualize_surface
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cross_section_init_1st(nod_fld)
!
      use m_nod_comm_table
      use m_ele_sf_eg_comm_tables
      use m_geometry_data
      use m_group_data
      use m_cross_section
!
      type(phys_data), intent(in) :: nod_fld
!
!
      call cross_section_init                                           &
     &   (node1, ele1, surf1, edge1, nod_comm, edge_comm,               &
     &    ele_grp1, sf_grp1, sf_grp_nod1, nod_fld)
!
      end subroutine cross_section_init_1st
!
!  ---------------------------------------------------------------------
!
      subroutine isosurface_init_1st(nod_fld)
!
      use m_geometry_data
      use m_group_data
      use m_isosurface
!
      type(phys_data), intent(in) :: nod_fld
!
!
      call isosurface_init                                              &
     &   (node1, ele1, surf1, edge1, ele_grp1, nod_fld)
!
      end subroutine isosurface_init_1st
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cross_section_main_1st(istep_psf, nod_fld)
!
      use m_geometry_data
      use m_cross_section
!
      integer(kind = kint), intent(in) :: istep_psf
      type(phys_data), intent(in) :: nod_fld
!
!
      call cross_section_main(istep_psf, edge1, nod_fld)
!
      end subroutine cross_section_main_1st
!
!  ---------------------------------------------------------------------
!
      subroutine isosurface_main_1st(istep_iso, nod_fld)
!
      use m_geometry_data
      use m_isosurface
      use m_ele_sf_eg_comm_tables
!
      integer(kind = kint), intent(in) :: istep_iso
      type(phys_data), intent(in) :: nod_fld
!
      call isosurface_main                                              &
     &   (istep_iso, node1, ele1, edge1, edge_comm, nod_fld)
!
      end subroutine isosurface_main_1st
!
!  ---------------------------------------------------------------------
!
      end module sections_for_1st
