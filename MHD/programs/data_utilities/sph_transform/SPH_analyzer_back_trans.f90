!SPH_analyzer_back_trans.f90
!     module SPH_analyzer_back_trans
!
!      Written by H. Matsui
!
!!      subroutine SPH_initialize_back_trans                            &
!!     &         (sph_mesh, ipol, idpdr, itor, rj_fld, fld_IO)
!!      subroutine SPH_analyze_back_trans                               &
!!     &         (i_step, sph_mesh, ipol, rj_fld, fld_IO, visval)
!
      module SPH_analyzer_back_trans
!
      use m_precision
      use m_machine_parameter
      use m_SPH_transforms
      use calypso_mpi
!
      use t_spheric_mesh
      use t_phys_address
      use t_phys_data
      use t_field_data_IO
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine SPH_initialize_back_trans                              &
     &         (sph_mesh, ipol, idpdr, itor, rj_fld, fld_IO)
!
      use m_t_step_parameter
      use m_ctl_params_sph_trans
      use m_node_id_spherical_IO
      use m_control_params_2nd_files
!
      use r_interpolate_sph_data
      use count_num_sph_smp
      use field_IO_select
      use set_phys_name_4_sph_trans
      use set_sph_phys_address
      use init_sph_trans
      use pole_sph_transform
      use legendre_transform_select
      use sph_transfer_all_field
!
      type(sph_mesh_data), intent(inout) :: sph_mesh
      type(phys_address), intent(inout) :: ipol, idpdr, itor
      type(phys_data), intent(inout) :: rj_fld
      type(field_IO), intent(inout) :: fld_IO
!
!
!  ------  initialize spectr data
!
      if (iflag_debug.gt.0) write(*,*) 'sel_read_alloc_step_fld_file'
      call set_field_file_fmt_prefix                                    &
     &   (ifmt_org_rst, org_rst_header, fld_IO)
      write(*,*) 'ifmt_org_rst', ifmt_org_rst
      call sel_read_alloc_step_SPH_file                                 &
     &   (nprocs, my_rank, i_step_init, fld_IO)
!
      if (iflag_debug.gt.0) write(*,*) 'copy_sph_name_rj_to_rtp'
      call copy_sph_name_rj_to_rtp(rj_fld)
!
!  ------    set original spectr modes
!
      if(iflag_org_sph_rj_head .gt. 0) then
        if (iflag_debug.gt.0) write(*,*) 'input_old_rj_sph_trans'
        call input_old_rj_sph_trans(my_rank,                            &
     &     sph_mesh%sph%sph_params%l_truncation, sph_mesh%sph%sph_rj)
        call set_sph_magne_address(rj_fld, ipol)
      end if
!
      call set_cmb_icb_radial_point(cmb_radial_grp, icb_radial_grp,     &
     &    sph_mesh%sph_grps%radial_rj_grp)
!
!  ---- allocate spectr data
!
      call set_sph_sprctr_data_address                                  &
     &   (sph_mesh%sph%sph_rj, ipol, idpdr, itor, rj_fld)
!
!  ---- initialize spherical harmonics transform
!
      if (iflag_debug.gt.0) write(*,*) 'initialize_sph_trans'
      if(id_legendre_transfer.eq.iflag_leg_undefined)                   &
     &            id_legendre_transfer = iflag_leg_orginal_loop
      call copy_sph_trans_nums_from_rtp(ncomp_sph_trans)
      call initialize_sph_trans(ncomp_sph_trans,                        &
     &    sph_mesh%sph, sph_mesh%sph_comms, trns_param)
!
      call init_pole_transform(sph_mesh%sph%sph_rtp)
      call allocate_d_pole_4_all_trans                                  &
     &   (ncomp_sph_trans, sph_mesh%sph%sph_rtp)
!
!      call calypso_MPI_barrier
!      call check_schmidt_poly_rtm(my_rank+40, sph_mesh%sph%sph_rtm,    &
!     &    sph_mesh%sph%sph_rlm, trns_param%leg)
!
      end subroutine SPH_initialize_back_trans
!
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_back_trans                                 &
     &         (i_step, sph_mesh, ipol, rj_fld, fld_IO, visval)
!
      use m_t_step_parameter
      use m_node_id_spherical_IO
      use m_control_params_2nd_files
!
      use field_IO_select
      use r_interpolate_sph_data
      use copy_rj_phys_data_4_IO
!
      use sph_transfer_all_field
      use set_exit_flag_4_visualizer
!
!
      integer(kind = kint), intent(in) :: i_step
      type(sph_mesh_data), intent(in) :: sph_mesh
      type(phys_address), intent(in) :: ipol
!
      integer(kind = kint), intent(inout) :: visval
      type(phys_data), intent(inout) :: rj_fld
      type(field_IO), intent(inout) :: fld_IO
!
      integer(kind = kint) :: i_udt
!
!
      call set_output_flag(i_udt, i_step, i_step_output_ucd)
      call set_output_flag_4_viz(i_step, visval)
      call set_field_file_fmt_prefix                                    &
     &   (ifmt_org_rst, org_rst_header, fld_IO)
      write(*,*) 'ifmt_org_rst', ifmt_org_rst
      visval = visval * i_udt
!
      if(visval .eq. 0) then
!
!   Input spectr data
        if (iflag_debug.gt.0) write(*,*) 'sel_read_step_SPH_field_file'
      call sel_read_step_SPH_field_file                                 &
     &     (nprocs, my_rank, i_step, fld_IO)
!
!    copy and extend magnetic field to outside
!
        if(iflag_org_sph_rj_head .eq. 0) then
          if (iflag_debug.gt.0) write(*,*) 'set_rj_phys_data_from_IO'
          call set_rj_phys_data_from_IO                                 &
     &       (sph_mesh%sph%sph_rj%nnod_rj, fld_IO, rj_fld)
        else
          if (iflag_debug.gt.0) write(*,*)                              &
     &                        'r_interpolate_sph_fld_from_IO'
          call r_interpolate_sph_fld_from_IO                            &
     &       (fld_IO, sph_mesh%sph%sph_rj, ipol, rj_fld)
        end if
!
!          call check_all_field_data(my_rank, rj_fld)
!  spherical transform for vector
        call sph_b_trans_all_field                                      &
     &     (ncomp_sph_trans, sph_mesh%sph, sph_mesh%sph_comms,          &
     &      femmesh_STR%mesh, trns_param, rj_fld, field_STR)
      end if
!
      end subroutine SPH_analyze_back_trans
!
! ----------------------------------------------------------------------
!
      end module SPH_analyzer_back_trans
