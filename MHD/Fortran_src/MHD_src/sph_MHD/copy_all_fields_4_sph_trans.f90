!>@file   copy_all_fields_4_sph_trans.f90
!!@brief  module copy_all_fields_4_sph_trans
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Copy spectrum data and field data to spherical transform buffer
!!       for dynamo simulation
!!
!!@verbatim
!!      subroutine copy_all_field_from_trans(m_folding, sph_rtp,       &
!!     &          trns_MHD, mesh, nod_fld)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(address_4_sph_trans), intent(in) :: trns_MHD
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(phys_data), intent(inout) :: nod_fld
!!
!!      subroutine copy_all_field_to_trans                              &
!!     &         (mesh, sph_rtp, nod_fld, trns_MHD)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(phys_data), intent(in) :: nod_fld
!!        type(address_4_sph_trans), intent(inout) :: trns_MHD
!!@endverbatim
!
      module copy_all_fields_4_sph_trans
!
      use m_precision
      use m_machine_parameter
      use m_phys_constants
      use copy_nodal_fld_4_sph_trans
!
      use t_sph_trans_comm_tbl
      use t_spheric_rtp_data
      use t_mesh_data
      use t_phys_address
      use t_phys_data
      use t_addresses_sph_transform
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_all_field_from_trans(m_folding, sph_rtp,       &
     &          trns_MHD, mesh, nod_fld)
!
      integer(kind = kint), intent(in) :: m_folding
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_4_sph_trans), intent(in) :: trns_MHD
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(inout) :: nod_fld
!
      integer(kind = kint) :: j, j_fld, i_fld
      integer(kind = kint) :: icomp, jcomp
!
!
!      write(*,*) 'trns_MHD%fld_rtp', trns_MHD%fld_rtp(1:sph_rtp%nnod_rtp,1:3)
!
      do j_fld = 1, trns_MHD%nvector_rj_2_rtp
        jcomp = n_vector*j_fld - 2
        do i_fld = 1, nod_fld%num_phys_viz
          if(nod_fld%phys_name(i_fld) .eq. trns_MHD%b_trns_name(j_fld)) &
     &        then
            icomp = nod_fld%istack_component(i_fld-1) + 1
!
            if(iflag_debug .gt. 0) write(*,*) 'copy field for  ',       &
     &               trim(nod_fld%phys_name(i_fld)), jcomp, icomp
            call copy_nod_vec_from_trans_wpole(sph_rtp, m_folding,      &
     &          trns_MHD%ncomp_rj_2_rtp, jcomp, trns_MHD%fld_rtp,       &
     &          trns_MHD%fld_pole, icomp, mesh%node, nod_fld)
          end if
        end do
      end do
!
      do j = 1, trns_MHD%nscalar_rj_2_rtp
        j_fld = j + trns_MHD%nvector_rj_2_rtp
        jcomp = j + n_vector*trns_MHD%nvector_rj_2_rtp
        do i_fld = 1, nod_fld%num_phys_viz
          if(nod_fld%phys_name(i_fld) .eq. trns_MHD%b_trns_name(j_fld)) &
     &        then
            icomp = nod_fld%istack_component(i_fld-1) + 1
!
            if(iflag_debug .gt. 0) write(*,*) 'copy field for  ',       &
     &               trim(nod_fld%phys_name(i_fld)), jcomp, icomp
            call copy_nod_scl_from_trans_wpole(sph_rtp, m_folding,      &
     &          trns_MHD%ncomp_rj_2_rtp, jcomp, trns_MHD%fld_rtp,       &
     &          trns_MHD%fld_pole, icomp, mesh%node, nod_fld)
          end if
        end do
      end do
!
      do j = 1, trns_MHD%ntensor_rj_2_rtp
        j_fld = j + trns_MHD%nvector_rj_2_rtp                           &
     &            + trns_MHD%nscalar_rj_2_rtp
        jcomp = n_sym_tensor * j - 5 + trns_MHD%nscalar_rj_2_rtp        &
     &         + n_vector*trns_MHD%nvector_rj_2_rtp
        do i_fld = 1, nod_fld%num_phys_viz
          if(nod_fld%phys_name(i_fld) .eq. trns_MHD%b_trns_name(j_fld)) &
     &        then
            icomp = nod_fld%istack_component(i_fld-1) + 1
!
            if(iflag_debug .gt. 0) write(*,*) 'copy field for  ',       &
     &               trim(nod_fld%phys_name(i_fld)), jcomp, icomp
            call copy_nod_tsr_from_trans_wpole(sph_rtp, m_folding,      &
     &          trns_MHD%ncomp_rj_2_rtp, jcomp, trns_MHD%fld_rtp,       &
     &          trns_MHD%fld_pole, icomp, mesh%node, nod_fld)
          end if
        end do
      end do
!
      end subroutine copy_all_field_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_all_field_to_trans                                &
     &         (mesh, sph_rtp, nod_fld, trns_MHD)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(in) :: nod_fld
      type(address_4_sph_trans), intent(inout) :: trns_MHD
!
      integer(kind = kint) :: j, j_fld, i_fld
      integer(kind = kint) :: icomp, jcomp
!
!
      do j_fld = 1, trns_MHD%nvector_rtp_2_rj
        jcomp = n_vector*j_fld - 2
        do i_fld = 1, nod_fld%num_phys_viz
          if(nod_fld%phys_name(i_fld) .eq. trns_MHD%f_trns_name(j_fld)) &
     &        then
            icomp = nod_fld%istack_component(i_fld-1) + 1
!
            if(iflag_debug .gt. 0) write(*,*) 'copy field for  ',       &
     &               trim(nod_fld%phys_name(i_fld)), jcomp, icomp
            call copy_nod_vec_to_sph_trans(mesh%node, sph_rtp,          &
     &          nod_fld, icomp, trns_MHD%frc_rtp(1,jcomp))
          end if
        end do
      end do
!
      do j = 1, trns_MHD%nscalar_rtp_2_rj
        j_fld = j + trns_MHD%nvector_rtp_2_rj
        jcomp = j + n_vector*trns_MHD%nvector_rtp_2_rj
        do i_fld = 1, nod_fld%num_phys_viz
          if(nod_fld%phys_name(i_fld) .eq. trns_MHD%f_trns_name(j_fld)) &
     &        then
            icomp = nod_fld%istack_component(i_fld-1) + 1
!
            if(iflag_debug .gt. 0) write(*,*) 'copy field for  ',       &
     &               trim(nod_fld%phys_name(i_fld)), jcomp, icomp
            call copy_nod_scl_to_sph_trans(mesh%node, sph_rtp,          &
     &          nod_fld, icomp, trns_MHD%frc_rtp(1,jcomp))
          end if
        end do
      end do
!
      do j = 1, trns_MHD%ntensor_rtp_2_rj
        j_fld = j + trns_MHD%nvector_rtp_2_rj                           &
     &            + trns_MHD%nscalar_rtp_2_rj
        jcomp = n_sym_tensor * j - 5 + trns_MHD%nscalar_rtp_2_rj        &
     &         + n_vector*trns_MHD%nvector_rtp_2_rj
        do i_fld = 1, nod_fld%num_phys_viz
          if(nod_fld%phys_name(i_fld) .eq. trns_MHD%f_trns_name(j_fld)) &
     &        then
            icomp = nod_fld%istack_component(i_fld-1) + 1
!
            if(iflag_debug .gt. 0) write(*,*) 'copy field for  ',       &
     &               trim(nod_fld%phys_name(i_fld)), jcomp, icomp
            call copy_nod_tsr_to_sph_trans(mesh%node, sph_rtp,          &
     &          nod_fld, icomp, trns_MHD%frc_rtp(1,jcomp))
          end if
        end do
      end do
!
      end subroutine copy_all_field_to_trans
!
!-----------------------------------------------------------------------
!
       end module copy_all_fields_4_sph_trans
