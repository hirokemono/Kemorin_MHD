!transfer_correlate_field.f90
!      module transfer_correlate_field
!
!     Written by H. Matsui on Nov., 2009
!
!      subroutine set_component_add_4_correlate(nod_fld)
!      subroutine coord_transfer_4_1st_field(nod_fld)
!      subroutine coord_transfer_4_2nd_field(node, nnod_2, phys_2nd)
!      subroutine copy_ref_component_to_2nd_fld(nod_fld, phys_2nd)
!      subroutine transfer_nod_fld_to_cyl(node,                         &
!     &          nnod, num_phys, ntot_phys, istack_component, d_nod)
!
      module transfer_correlate_field
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_geometry_constants
!
      use t_geometry_data
      use t_phys_data
!
      implicit none
!
      private :: transfer_nod_fld_to_sph, transfer_nod_fld_to_cyl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_component_add_4_correlate(nod_fld)
!
      use calypso_mpi
      use m_error_IDs
      use m_ctl_params_4_diff_udt
      use set_components_flags
!
      type(phys_data), intent(in) :: nod_fld
!
      integer(kind = kint) :: ifld, icomp, ncomp, ncomp_org
      character(len=kchara) :: field_comp_name
!
!
      i_field_4_correlate = 0
      do ifld = 1, nod_fld%num_phys
        if(correlate_field_name .eq. nod_fld%phys_name(ifld)) then
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
      icomp_4_correlate                                                 &
     &          = nod_fld%istack_component(i_field_4_correlate-1)       &
     &              + mod(icomp,iten)
      if(icomp .eq. icomp_CYLINDER_R) then
        icomp_4_correlate                                               &
     &          = nod_fld%istack_component(i_field_4_correlate-1) + 1
      end if
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &        'icomp_4_correlate', icomp_4_correlate
!
      end subroutine set_component_add_4_correlate
!
!  ---------------------------------------------------------------------
!
      subroutine coord_transfer_4_1st_field(node, nod_fld)
!
      use m_ctl_params_4_diff_udt
!
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      if     (iflag_correlate_coord .eq. iflag_spherical) then
        call transfer_nod_fld_to_sph                                    &
     &     (node, nod_fld%n_point, nod_fld%num_phys,                    &
     &      nod_fld%ntot_phys, nod_fld%istack_component, nod_fld%d_fld)
      else if(iflag_correlate_coord .eq. iflag_cylindrical) then
        call transfer_nod_fld_to_cyl                                    &
     &     (node, nod_fld%n_point, nod_fld%num_phys,                    &
     &      nod_fld%ntot_phys, nod_fld%istack_component, nod_fld%d_fld)
      end if
!
      end subroutine coord_transfer_4_1st_field
!
!  ---------------------------------------------------------------------
!
      subroutine coord_transfer_4_2nd_field(node, nnod_2, phys_2nd)
!
      use m_ctl_params_4_diff_udt
!
      type(node_data), intent(in) :: node
      integer(kind = kint), intent(in) :: nnod_2
      type(phys_data), intent(inout) :: phys_2nd
!
!
      if     (iflag_correlate_coord .eq. iflag_spherical) then
        call transfer_nod_fld_to_sph(node, nnod_2, phys_2nd%num_phys,   &
     &      phys_2nd%ntot_phys, phys_2nd%istack_component,              &
     &      phys_2nd%d_fld)
      else if(iflag_correlate_coord .eq. iflag_cylindrical) then
        call transfer_nod_fld_to_cyl(node, nnod_2, phys_2nd%num_phys,   &
     &      phys_2nd%ntot_phys, phys_2nd%istack_component,              &
     &      phys_2nd%d_fld)
      end if
!
      end subroutine coord_transfer_4_2nd_field
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine copy_ref_component_to_2nd_fld(node, nod_fld, phys_2nd)
!
      use m_ctl_params_4_diff_udt
!
      type(node_data), intent(in) :: node
      type(phys_data), intent(in) :: nod_fld
      type(phys_data), intent(inout) :: phys_2nd
      integer(kind = kint) :: inod, nd
!
!
!$omp parallel private(nd)
      do nd = 1, phys_2nd%ntot_phys
!$omp do 
        do inod = 1, node%numnod
          phys_2nd%d_fld(inod,nd)                                       &
     &       = nod_fld%d_fld(inod,icomp_4_correlate)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine copy_ref_component_to_2nd_fld
!
!  ---------------------------------------------------------------------
!
      subroutine transfer_nod_fld_to_sph(node,                          &
     &          nnod, num_phys, ntot_phys, istack_component, d_nod)
!
      use m_phys_constants
      use cvt_xyz_vector_2_sph_smp
      use cvt_xyz_tensor_2_sph_smp
!
      type(node_data), intent(in) :: node
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
          call overwrite_vector_2_sph_smp                               &
     &       (np_smp, node%numnod, node%istack_nod_smp,                 &
     &        d_nod(1,ist), node%xx(1:node%numnod,1),                   &
     &        node%xx(1:node%numnod,2), node%xx(1:node%numnod,3),       &
     &        node%rr, node%ss, node%a_r, node%a_s)
        else if(ncomp .eq. n_sym_tensor) then
          call overwrite_sph_tensor_smp                                 &
     &       (np_smp, node%numnod, node%istack_nod_smp,                 &
     &        d_nod(1,ist), node%xx(1:node%numnod,1),                   &
     &        node%xx(1:node%numnod,2), node%xx(1:node%numnod,3),       &
     &        node%rr, node%ss, node%a_r, node%a_s)
        end if
!$omp end parallel
      end do
!
      end subroutine transfer_nod_fld_to_sph
!
!  ---------------------------------------------------------------------
!
      subroutine transfer_nod_fld_to_cyl(node,                          &
     &          nnod, num_phys, ntot_phys, istack_component, d_nod)
!
      use m_phys_constants
      use cvt_xyz_vector_2_cyl_smp
      use cvt_xyz_tensor_2_cyl_smp
!
      type(node_data), intent(in) :: node
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
          call overwrite_vector_2_cyl_smp                               &
     &       (np_smp, node%numnod, node%istack_nod_smp, d_nod(1,ist),   &
     &        node%xx(1:node%numnod,1), node%xx(1:node%numnod,2),       &
     &        node%ss, node%a_s)
       else if(ncomp .eq. n_sym_tensor) then
          call overwrite_cyl_tensor_smp                                 &
     &       (np_smp, node%numnod, node%istack_nod_smp, d_nod(1,ist),   &
     &        node%xx(1:node%numnod,1), node%xx(1:node%numnod,2),       &
     &        node%ss, node%a_s)
        end if
!$omp end parallel
      end do
!
      end subroutine transfer_nod_fld_to_cyl
!
!  ---------------------------------------------------------------------
!
      end module transfer_correlate_field
