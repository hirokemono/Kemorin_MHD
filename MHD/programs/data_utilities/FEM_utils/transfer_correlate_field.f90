!transfer_correlate_field.f90
!      module transfer_correlate_field
!
!     Written by H. Matsui on Nov., 2009
!
!      subroutine set_component_add_4_correlate
!      subroutine coord_transfer_4_1st_field
!      subroutine coord_transfer_4_2nd_field
!
      module transfer_correlate_field
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_geometry_constants
      use m_geometry_data
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_component_add_4_correlate
!
      use calypso_mpi
      use m_error_IDs
      use m_ctl_params_4_diff_udt
      use m_node_phys_data
      use set_components_flags
!
      integer(kind = kint) :: ifld, icomp, ncomp, ncomp_org
      character(len=kchara) :: field_comp_name
!
!
      i_field_4_correlate = 0
      do ifld = 1, num_nod_phys
        if(correlate_field_name .eq. phys_nod_name(ifld)) then
          i_field_4_correlate = ifld
          exit
        end if
      end do
!
      if(i_field_4_correlate .eq. 0) then
        call calypso_MPI_abort(ierr_file, 'set correct field name')
      end if
!
      call s_set_components_flags(correlate_comp_name,                  &
     &    correlate_field_name, icomp, ncomp, ncomp_org,                &
     &    field_comp_name)
!
      icomp_4_correlate = istack_nod_component(i_field_4_correlate-1)   &
     &                   + mod(icomp,iten)
      if(icomp .eq. icomp_CYLINDER_R) then
        icomp_4_correlate = istack_nod_component(i_field_4_correlate-1) &
     &                     + 1
      end if
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &        'icomp_4_correlate', icomp_4_correlate
!
      end subroutine set_component_add_4_correlate
!
!  ---------------------------------------------------------------------
!
      subroutine coord_transfer_4_1st_field
!
      use m_ctl_params_4_diff_udt
      use m_node_phys_data
!
!
      if     (iflag_correlate_coord .eq. iflag_spherical) then
        call transfer_nod_fld_to_sph(node1%numnod, num_nod_phys,        &
     &      num_tot_nod_phys, istack_nod_component, d_nod)
      else if(iflag_correlate_coord .eq. iflag_cylindrical) then
        call transfer_nod_fld_to_cyl(node1%numnod, num_nod_phys,        &
     &     num_tot_nod_phys, istack_nod_component, d_nod)
      end if
!
!
      end subroutine coord_transfer_4_1st_field
!
!  ---------------------------------------------------------------------
!
      subroutine coord_transfer_4_2nd_field(nnod_2, phys_2nd)
!
      use m_ctl_params_4_diff_udt
      use t_phys_data
!
      integer(kind = kint), intent(in) :: nnod_2
      type(phys_data), intent(inout) :: phys_2nd
!
!
      if     (iflag_correlate_coord .eq. iflag_spherical) then
        call transfer_nod_fld_to_sph(nnod_2, phys_2nd%num_phys,         &
     &      phys_2nd%ntot_phys, phys_2nd%istack_component,              &
     &      phys_2nd%d_fld)
      else if(iflag_correlate_coord .eq. iflag_cylindrical) then
        call transfer_nod_fld_to_cyl(nnod_2, phys_2nd%num_phys,         &
     &      phys_2nd%ntot_phys, phys_2nd%istack_component,              &
     &      phys_2nd%d_fld)
      end if
!
      end subroutine coord_transfer_4_2nd_field
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine copy_ref_component_to_2nd_fld(phys_2nd)
!
      use m_ctl_params_4_diff_udt
      use m_node_phys_data
      use t_phys_data
!
      type(phys_data), intent(in) :: phys_2nd
      integer(kind = kint) :: inod, nd
!
!
!$omp parallel private(nd)
      do nd = 1, phys_2nd%ntot_phys
!$omp do 
        do inod = 1, node1%numnod
          phys_2nd%d_fld(inod,nd) = d_nod(inod,icomp_4_correlate)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine copy_ref_component_to_2nd_fld
!
!  ---------------------------------------------------------------------
!
      subroutine transfer_nod_fld_to_sph(nnod, num_phys, ntot_phys,     &
     &          istack_component, d_nod)
!
      use m_phys_constants
      use cvt_xyz_vector_2_sph_smp
      use cvt_xyz_tensor_2_sph_smp
!
      integer(kind = kint), intent(in) :: nnod, num_phys, ntot_phys
      integer(kind = kint), intent(in) :: istack_component(0:num_phys)
      real(kind = kreal), intent(inout) :: d_nod(nnod,ntot_phys)
!
      integer(kind = kint) :: i_fld, ist, ncomp
!
!
      do i_fld = 1, num_phys
        ist = istack_component(i_fld-1) + 1
        ncomp = istack_component(i_fld) - istack_component(i_fld-1)
!$omp parallel
        if     (ncomp .eq. n_vector) then
          call overwrite_vector_2_sph_smp(np_smp, node1%numnod,         &
     &        inod_smp_stack, d_nod(1,ist), xx(1,1), xx(1,2), xx(1,3),  &
     &        radius, s_cylinder, a_radius, a_s_cylinder)
        else if(ncomp .eq. n_sym_tensor) then
          call overwrite_sph_tensor_smp(np_smp, node1%numnod,           &
     &        inod_smp_stack, d_nod(1,ist), xx(1,1), xx(1,2), xx(1,3),  &
     &        radius, s_cylinder, a_radius, a_s_cylinder)
        end if
!$omp end parallel
      end do
!
      end subroutine transfer_nod_fld_to_sph
!
!  ---------------------------------------------------------------------
!
      subroutine transfer_nod_fld_to_cyl(nnod, num_phys, ntot_phys,     &
     &          istack_component, d_nod)
!
      use m_phys_constants
      use cvt_xyz_vector_2_cyl_smp
      use cvt_xyz_tensor_2_cyl_smp
!
      integer(kind = kint), intent(in) :: nnod, num_phys, ntot_phys
      integer(kind = kint), intent(in) :: istack_component(0:num_phys)
      real(kind = kreal), intent(inout) :: d_nod(nnod,ntot_phys)
!
      integer(kind = kint) :: i_fld, ist, ncomp
!
!
      do i_fld = 1, num_phys
        ist = istack_component(i_fld-1) + 1
        ncomp = istack_component(i_fld) - istack_component(i_fld-1)
!$omp parallel
        if     (ncomp .eq. n_vector) then
          call overwrite_vector_2_cyl_smp(np_smp, node1%numnod,         &
     &          inod_smp_stack, d_nod(1,ist),                           &
     &          xx(1,1), xx(1,2), s_cylinder, a_s_cylinder)
       else if(ncomp .eq. n_sym_tensor) then
          call overwrite_cyl_tensor_smp(np_smp, node1%numnod,           &
     &          inod_smp_stack, d_nod(1,ist), xx(1,1), xx(1,2),         &
     &          s_cylinder, a_s_cylinder)
        end if
!$omp end parallel
      end do
!
      end subroutine transfer_nod_fld_to_cyl
!
!  ---------------------------------------------------------------------
!
      end module transfer_correlate_field
