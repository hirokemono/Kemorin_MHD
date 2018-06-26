!set_fields_for_fieldline.f90
!
!      module set_fields_for_fieldline
!
!      Written by H. Matsui on Aug., 2011
!
!!      subroutine set_local_field_4_fline                              &
!!     &          (num_fline, node, nod_fld, fline_prm, fline_src)
!!        type(node_data), intent(in) :: node
!!      subroutine count_nsurf_for_starting                             &
!!     &         (i_fln, ele, sf_grp, fline_prm, fline_src)
!!      subroutine set_isurf_for_starting                               &
!!     &         (i_fln, ele, sf_grp, fline_prm, fline_src)
!!        type(element_data), intent(in) :: ele
!!      subroutine s_set_fields_for_fieldline                           &
!!     &         (i_fln, mesh, ele_mesh, group,                         &
!!     &          fln_prm, fline_prm, fline_src, fline_tce)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(element_geometry), intent(in) :: ele_mesh
!!        type(mesh_groups), intent(in) :: group
!!        type(fieldline_paramter), intent(in) :: fln_prm
!!        type(fieldline_paramters), intent(inout) :: fline_prm
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(fieldline_source), intent(inout) :: fline_src
!!        type(fieldline_trace), intent(inout) :: fline_tce
!
      module set_fields_for_fieldline
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_machine_parameter
!
      use t_phys_data
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_control_params_4_fline
      use t_source_of_filed_line
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_local_field_4_fline                                &
     &          (num_fline, node, nod_fld, fline_prm, fline_src)
!
      use convert_components_4_viz
!
      integer(kind = kint) :: num_fline
      type(node_data), intent(in) :: node
      type(phys_data), intent(in) :: nod_fld
      type(fieldline_paramters), intent(in) :: fline_prm
!
      type(fieldline_source), intent(inout) :: fline_src
!
      integer(kind = kint) :: i_fln
      integer(kind = kint) :: i_field, ist_fld, num_comp
!
!
      do i_fln = 1, num_fline
        i_field = fline_prm%ifield_4_fline(i_fln)
        ist_fld = nod_fld%istack_component(i_field-1)
        num_comp = nod_fld%istack_component(i_field) - ist_fld
!
        if (iflag_debug .gt. 0) write(*,*)                              &
     &    'convert_comps_4_viz ifield_4_fline', i_field
        call convert_comps_4_viz(node%numnod, node%istack_nod_smp,      &
     &      node%xx, node%rr,node%a_r, node%ss, node%a_s, ithree,       &
     &      num_comp, fline_prm%icomp_4_fline(i_fln),                   &
     &      nod_fld%d_fld(1,ist_fld+1),                                 &
     &      fline_src%vector_nod_fline(1,1,i_fln) )
!
        i_field = fline_prm%ifield_linecolor(i_fln)
        ist_fld = nod_fld%istack_component(i_field-1)
        num_comp = nod_fld%istack_component(i_field) - ist_fld
        if (iflag_debug .gt. 0) write(*,*)                              &
     &     'convert_comps_4_viz ifield_linecolor', i_field
        call convert_comps_4_viz(node%numnod, node%istack_nod_smp,      &
     &      node%xx, node%rr, node%a_r, node%ss, node%a_s, ione,        &
     &      num_comp, fline_prm%icomp_linecolor(i_fln),                 &
     &      nod_fld%d_fld(1,ist_fld+1),                                 &
     &      fline_src%color_nod_fline(1,i_fln) )
      end do
!
!
      end subroutine set_local_field_4_fline
!
!  ---------------------------------------------------------------------
!
      subroutine count_nsurf_for_starting                               &
     &         (i_fln, ele, sf_grp, fline_prm, fline_src)
!
      integer(kind = kint), intent(in) :: i_fln
!
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: sf_grp
      type(fieldline_paramters), intent(in) :: fline_prm
!
      type(fieldline_source), intent(inout) :: fline_src
!
      integer(kind = kint) :: igrp, isurf, iele, icou, ist, ied
!
!
      igrp = fline_prm%igrp_start_fline_surf_grp(i_fln)
!
      icou = 0
      ist = sf_grp%istack_grp(igrp-1) + 1
      ied = sf_grp%istack_grp(igrp)
      do isurf = ist, ied
        iele = sf_grp%item_sf_grp(1,isurf)
        if(ele%interior_ele(iele) .ne. izero) icou = icou + 1
      end do
!
      fline_src%nele_start_grp(i_fln) = icou
      fline_src%istack_ele_start_grp(i_fln)                             &
     &             = fline_src%istack_ele_start_grp(i_fln-1)            &
     &              + fline_src%nele_start_grp(i_fln)
!
!
      end subroutine count_nsurf_for_starting
!
!  ---------------------------------------------------------------------
!
      subroutine set_isurf_for_starting                                 &
     &         (i_fln, ele, sf_grp, fline_prm, fline_src)
!
      integer(kind = kint), intent(in) :: i_fln
!
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: sf_grp
      type(fieldline_paramters), intent(in) :: fline_prm
!
      type(fieldline_source), intent(inout) :: fline_src
!
      integer(kind = kint) :: igrp, isurf, inum, iele, ist, ied
!
!
      igrp = fline_prm%igrp_start_fline_surf_grp(i_fln)
!
      inum = fline_src%istack_ele_start_grp(i_fln-1)
      ist = sf_grp%istack_grp(igrp-1) + 1
      ied = sf_grp%istack_grp(igrp)
      do isurf = ist, ied
        iele = sf_grp%item_sf_grp(1,isurf)
        if(ele%interior_ele(iele) .ne. izero) then
          inum = inum + 1
          fline_src%iele_start_item(1,inum)                             &
     &         = sf_grp%item_sf_grp(1,isurf)
          fline_src%iele_start_item(2,inum)                             &
     &         = sf_grp%item_sf_grp(2,isurf)
        end if
      end do
!
      end subroutine set_isurf_for_starting
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_fields_for_fieldline                             &
     &         (i_fln, mesh, ele_mesh, group,                           &
     &          fln_prm, fline_prm, fline_src, fline_tce)
!
      use t_mesh_data
      use start_surface_by_gl_table
      use start_surface_by_flux
      use start_surface_in_volume
      use start_surface_4_fline
!
      integer(kind = kint), intent(in) :: i_fln
!
      type(mesh_geometry), intent(in) :: mesh
      type(element_geometry), intent(in) :: ele_mesh
      type(mesh_groups), intent(in) :: group
      type(fieldline_paramter), intent(in) :: fln_prm
!
      type(fieldline_paramters), intent(inout) :: fline_prm
      type(fieldline_source), intent(inout) :: fline_src
      type(fieldline_trace), intent(inout) :: fline_tce
!
!
      if(fln_prm%id_fline_seed_type .eq. iflag_surface_group) then
        if(iflag_debug .gt. 0) write(*,*) 's_start_surface_by_flux'
        call s_start_surface_by_flux                                    &
     &     (i_fln, mesh%node, mesh%ele, ele_mesh%surf,                  &
     &      fline_prm, fline_src, fline_tce)
      else if(fln_prm%id_fline_seed_type                                &
     &                           .eq. iflag_spray_in_domain) then
        if(iflag_debug .gt. 0) write(*,*) 's_start_surface_by_volume'
        call s_start_surface_by_volume(i_fln, mesh%ele, group%ele_grp,  &
     &      fline_prm, fline_src, fline_tce)
      else if(fln_prm%id_fline_seed_type .eq. iflag_surface_list) then
        if(iflag_debug .gt. 0) write(*,*) 's_start_surface_by_gl_table'
        call s_start_surface_by_gl_table                                &
     &     (i_fln, mesh%ele, group%ele_grp, fline_prm, fline_src)
      else if(fln_prm%id_fline_seed_type                                &
     &                           .eq. iflag_spray_in_domain) then
      end if
!
      if(iflag_debug .gt. 0) write(*,*) 's_start_surface_4_fline'
      call s_start_surface_4_fline                                      &
     &   (i_fln, mesh%node, mesh%ele, ele_mesh%surf,                    &
     &    fline_prm, fline_src, fline_tce)
!
      end subroutine s_set_fields_for_fieldline
!
!  ---------------------------------------------------------------------
!
      end module set_fields_for_fieldline
