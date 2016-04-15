!r_interpolate_sph_data.f90
!     module r_interpolate_sph_data
!
!      Written by H. Matsui on Sep., 2011
!
!      subroutine deallocate_original_sph_data
!
!!      subroutine copy_cmb_icb_radial_point
!!      subroutine set_cmb_icb_radial_point(cmb_r_grp, icb_r_grp)
!!      subroutine set_sph_magne_address(rj_fld)
!!      subroutine input_old_rj_sph_trans(my_rank)
!!
!!      subroutine r_interpolate_sph_rst_from_IO(fld_IO, rj_fld)
!!      subroutine r_interpolate_sph_fld_from_IO(fld_IO, rj_fld)
!!      subroutine set_poloidal_b_by_gauss_coefs(rj_fld)
!!        type(phys_data), intent(inout) :: rj_fld
!
      module r_interpolate_sph_data
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_machine_parameter
      use m_spheric_parameter
!
      implicit  none
!
!
      integer(kind = kint) :: kr_inside, kr_outside
!
      integer(kind = kint) :: nri_org, n_rj_org
      integer(kind = kint), allocatable :: k_inter(:,:)
      real(kind = kreal), allocatable :: rcoef_inter(:,:)
!
      real(kind = kreal), allocatable :: r_org(:)
!
      integer(kind = kint) :: ntot_phys_rj_itp
      real(kind = kreal), allocatable :: d_rj_org(:,:)
!
      private :: allocate_original_sph_data
      private :: copy_original_sph_rj_from_IO
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine copy_cmb_icb_radial_point
!
!
      kr_outside = nlayer_CMB
      kr_inside =  nlayer_ICB
!
      end subroutine copy_cmb_icb_radial_point
!
!  -------------------------------------------------------------------
!
      subroutine set_cmb_icb_radial_point(cmb_r_grp, icb_r_grp)
!
      use m_group_data_sph_specr
!
      character(len = kchara), intent(in) :: cmb_r_grp, icb_r_grp
      integer(kind = kint) :: igrp, inum
!
!
      kr_outside = 0
      do igrp = 1, num_radial_grp_rj
        if(name_radial_grp_rj(igrp) .eq. cmb_r_grp) then
          inum = istack_radial_grp_rj(igrp-1) + 1
          kr_outside = item_radial_grp_rj(inum)
          exit
        end if
      end do
!
      kr_inside = 0
      do igrp = 1, num_radial_grp_rj
        if(name_radial_grp_rj(igrp) .eq. icb_r_grp) then
          inum = istack_radial_grp_rj(igrp-1) + 1
          kr_inside = item_radial_grp_rj(inum)
          exit
        end if
      end do
!
      end subroutine set_cmb_icb_radial_point
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine allocate_original_sph_data
!
!
      allocate(r_org(nri_org))
      allocate(k_inter(nri_org,2))
      allocate(rcoef_inter(nri_org,2))
!
      allocate(d_rj_org(n_rj_org,6))
!
      k_inter = izero
      r_org = zero
      rcoef_inter = zero
      d_rj_org =    zero
!
      end subroutine allocate_original_sph_data
!
!  -------------------------------------------------------------------
!
      subroutine deallocate_original_sph_data
!
      deallocate(r_org, d_rj_org)
      deallocate(k_inter, rcoef_inter)
!
      end subroutine deallocate_original_sph_data
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine set_sph_magne_address(rj_fld)
!
      use m_phys_labels
      use m_sph_phys_address
      use t_phys_data
!
      type(phys_data), intent(in) :: rj_fld
      integer(kind = kint) :: i
!
      do i = 1, rj_fld%num_phys
        if(rj_fld%phys_name(i) .eq. fhd_magne) then
          ipol%i_magne = rj_fld%istack_component(i-1) + 1
          exit
        end if
      end do
!
      end subroutine set_sph_magne_address
!
!  -------------------------------------------------------------------
!
      subroutine input_old_rj_sph_trans(my_rank)
!
      use m_node_id_spherical_IO
      use m_control_params_2nd_files
      use sph_file_IO_select
      use radial_interpolation
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_sph_mesh_file_fmt_prefix                                 &
     &   (ifmt_org_sph_rj_head, org_sph_rj_head)
      call sel_read_spectr_modes_rj_file(my_rank)
      call copy_original_sph_rj_from_IO
!
      call const_radial_itp_table(nri_org, r_org,                       &
     &    nidx_rj(1), sph_rj1%radius_1d_rj_r, kr_inside, kr_outside,    &
     &    k_inter, rcoef_inter)
!
      end subroutine input_old_rj_sph_trans
!
!  -------------------------------------------------------------------
!
      subroutine r_interpolate_sph_rst_from_IO(fld_IO, rj_fld)
!
      use m_phys_labels
      use m_sph_phys_address
      use t_phys_data
      use t_field_data_IO
      use extend_potential_field
      use radial_interpolation
!
      type(field_IO), intent(in) :: fld_IO
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: i_fld, j_fld
!
!
      do i_fld = 1, rj_fld%ntot_phys
        do j_fld = 1, fld_IO%num_field_IO
          if(rj_fld%phys_name(i_fld) .eq. fld_IO%fld_name(j_fld)) then
            if     (rj_fld%phys_name(i_fld) .eq. fhd_velo               &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_vort               &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_press              &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_temp               &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_light              &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_magne              &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_mag_potential      &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_entropy            &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_pre_mom            &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_pre_uxb            &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_pre_heat           &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_pre_composit       &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_heat_source        &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_light_source       &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_entropy_source     &
     &         ) then
              call set_org_rj_phys_data_from_IO                         &
     &           (j_fld, fld_IO, n_rj_org, d_rj_org)
              call r_interpolate_sph_vector(i_fld, nidx_rj,             &
     &            rj_fld%n_point, rj_fld%num_phys, rj_fld%ntot_phys,    &
     &            rj_fld%istack_component, kr_inside, kr_outside,       &
     &            nri_org, k_inter, rcoef_inter, n_rj_org, d_rj_org,    &
     &            rj_fld%d_fld)
              exit
            end if
          end if
        end do
      end do
!
      if (ipol%i_magne .gt. 0) then
        call ext_outside_potential(kr_outside, ipol%i_magne,            &
     &      nidx_rj, idx_gl_1d_rj_j,    &
     &      sph_rj1%radius_1d_rj_r, sph_rj1%a_r_1d_rj_r,                &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        call ext_inside_potential(kr_inside, ipol%i_magne,              &
     &      nidx_rj, idx_gl_1d_rj_j,    &
     &      sph_rj1%radius_1d_rj_r, sph_rj1%a_r_1d_rj_r,                &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine r_interpolate_sph_rst_from_IO
!
! -------------------------------------------------------------------
!
      subroutine r_interpolate_sph_fld_from_IO(fld_IO, rj_fld)
!
      use m_sph_phys_address
      use t_phys_data
      use t_field_data_IO
      use extend_potential_field
      use radial_interpolation
!
      type(field_IO), intent(in) :: fld_IO
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) ::  i_fld, j_fld
!
!
      do i_fld = 1, rj_fld%ntot_phys
        do j_fld = 1, fld_IO%num_field_IO
          if(rj_fld%phys_name(i_fld) .eq. fld_IO%fld_name(j_fld)) then
            call set_org_rj_phys_data_from_IO                           &
     &         (j_fld, fld_IO, n_rj_org, d_rj_org)
            call r_interpolate_sph_vector(i_fld, nidx_rj,               &
     &          rj_fld%n_point, rj_fld%num_phys, rj_fld%ntot_phys,      &
     &          rj_fld%istack_component,  kr_inside, kr_outside,        &
     &          nri_org, k_inter, rcoef_inter, n_rj_org, d_rj_org,      &
     &          rj_fld%d_fld)
            exit
          end if
        end do
      end do
!
      if (ipol%i_magne .gt. 0) then
        call ext_outside_potential(kr_outside, ipol%i_magne,            &
     &      nidx_rj, idx_gl_1d_rj_j,    &
     &      sph_rj1%radius_1d_rj_r, sph_rj1%a_r_1d_rj_r,                &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        call ext_inside_potential(kr_inside, ipol%i_magne,              &
     &      nidx_rj, idx_gl_1d_rj_j,     &
     &      sph_rj1%radius_1d_rj_r, sph_rj1%a_r_1d_rj_r,                &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine r_interpolate_sph_fld_from_IO
!
! -----------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine set_poloidal_b_by_gauss_coefs(rj_fld)
!
      use m_sph_phys_address
      use m_global_gauss_coefs
      use t_phys_data
      use extend_potential_field
!
      type(phys_data), intent(in) :: rj_fld
!
!
      write(*,*) ' ipol%i_magne', ipol%i_magne, kr_outside, kr_inside
      if (ipol%i_magne .gt. 0) then
        call gauss_to_poloidal_out(kr_outside, ltr_w, r_gauss,          &
     &      w_gauss, index_w, ipol%i_magne, sph_rj1%a_r_1d_rj_r,        &
     &      rj_fld%ntot_phys, rj_fld%d_fld)
        call gauss_to_poloidal_in(kr_inside, ltr_w, r_gauss,            &
     &      w_gauss, index_w, ipol%i_magne, sph_rj1%radius_1d_rj_r,     &
     &      rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine set_poloidal_b_by_gauss_coefs
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_original_sph_rj_from_IO
!
      use m_error_IDs
      use m_node_id_spherical_IO
      use m_comm_data_IO
      use m_group_data_sph_specr_IO
!
!
      if(sph_rank_rj(1).ne.sph_rank_IO(1)                               &
     &       .or. sph_rank_rj(2).ne.sph_rank_IO(2)) then
        call calypso_MPI_abort(ierr_sph,'rj rank ID is wrong')
      end if
!
      if(nidx_global_rj(2) .ne. nidx_gl_sph_IO(2)) then
        call calypso_MPI_abort                                          &
     &     (ierr_sph,'number of local mode is wrong')
      end if
      if(l_truncation .ne. ltr_gl_IO) then
        call calypso_MPI_abort(ierr_sph,'truncation is wrong')
      end if
!
      if(ist_rj(2).ne.ist_sph_IO(2)) then
        call calypso_MPI_abort                                          &
     &      (ierr_sph,'start point of harminics is wrong')
      end if
      if(ied_rj(2).ne.ied_sph_IO(2)) then
        call calypso_MPI_abort                                          &
     &     (ierr_sph,'end point of harminics is wrong')
      end if
!
      n_rj_org = nnod_sph_IO
      nri_org =  nidx_sph_IO(1)
!
      call allocate_original_sph_data
!
      r_org(1:n_rj_org) =   r_gl_1_IO(1:n_rj_org)
!
      call deallocate_nod_id_sph_IO
      call deallocate_idx_sph_1d1_IO
      call deallocate_idx_sph_1d2_IO
!
!
      call deallocate_import_item_IO
      call deallocate_neib_domain_IO
!
      call deallocate_rj_r_grp_IO_item
      call deallocate_rj_j_grp_IO_item
!
      end subroutine copy_original_sph_rj_from_IO
!
! ----------------------------------------------------------------------
!
      end module r_interpolate_sph_data
