!copy_all_spec_4_sph_trans.f90
!      module copy_all_spec_4_sph_trans
!
!        programmed by H.Matsui on Jan., 2008
!
!!      subroutine set_all_scalar_spec_to_sph_t                         &
!!     &         (nnod_pole, ncomp_send, sph_rj, comm_rj,               &
!!     &          fld_rtp, rj_fld, n_WS, WS, v_pl_local)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(sph_comm_tbl), intent(in) :: comm_rj
!!        type(phys_data), intent(in) :: rj_fld
!!      subroutine set_all_scalar_spec_from_sph_t(iflag_recv,           &
!!     &          ncomp_recv, comm_rj, fld_rtp, n_WR, WR, rj_fld)
!!        type(phys_data), intent(inout) :: rj_fld
!!
!!      subroutine set_all_vec_spec_to_sph_t                            &
!!     &         (ncomp_send, comm_rj, fld_rtp, rj_fld, n_WS, WS)
!!      subroutine set_all_vec_spec_from_sph_t(iflag_recv,               
!!     &          ncomp_recv, comm_rj, fld_rtp, n_WR, WR, rj_fld)
!!
!!      subroutine set_all_tensor_spec_to_sph_t                         &
!!     &         (ncomp_send, comm_rj, fld_rtp, rj_fld, n_WS, WS)
!!      subroutine set_all_tensor_spec_from_sph_t(iflag_recv,           &
!!     &          ncomp_recv, comm_rj, fld_rtp, n_WR, WR, rj_fld)
!
      module copy_all_spec_4_sph_trans
!
      use m_precision
!
      use t_sph_trans_comm_tbl
      use t_phys_data
!
      use t_phys_name_4_sph_trans
!
      implicit  none
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine set_all_scalar_spec_to_sph_t                           &
     &         (nnod_pole, ncomp_send, sph_rj, comm_rj,                 &
     &          fld_rtp, rj_fld, n_WS, WS, v_pl_local)
!
      use t_spheric_rj_data
      use copy_spectr_4_sph_trans
!
      integer(kind = kint), intent(in) :: ncomp_send, n_WS, nnod_pole
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_comm_tbl), intent(in) :: comm_rj
      type(field_name_4_sph_trans), intent(in) :: fld_rtp
      type(phys_data), intent(in) :: rj_fld
      real(kind = kreal), intent(inout) :: WS(n_WS)
      real(kind = kreal), intent(inout)                                 &
     &                :: v_pl_local(nnod_pole,ncomp_send)
!
      integer(kind = kint) :: i, j, j0, i_field, itrans
!
!
      do j = 1, fld_rtp%num_scalar
        j0 = j + fld_rtp%istart_scalar_rtp - 1
        itrans = j+3*fld_rtp%num_vector
        do i = 1, rj_fld%num_phys
          if (fld_rtp%fld_name(j0) .eq. rj_fld%phys_name(i) ) then
            i_field = rj_fld%istack_component(i-1) + 1
            call sel_sph_rj_scalar_2_send_wpole                         &
     &         (ncomp_send, i_field, itrans, nnod_pole,                 &
     &          sph_rj, comm_rj, rj_fld, n_WS, WS, v_pl_local)
            exit
          end if
        end do
      end do
!
      end subroutine set_all_scalar_spec_to_sph_t
!
! -------------------------------------------------------------------
!
      subroutine set_all_scalar_spec_from_sph_t(iflag_recv,             &
     &          ncomp_recv, comm_rj, fld_rtp, n_WR, WR, rj_fld)
!
      use copy_spectr_4_sph_trans
!
      integer(kind = kint), intent(in) :: iflag_recv
      type(sph_comm_tbl), intent(in) :: comm_rj
      type(field_name_4_sph_trans), intent(in) :: fld_rtp
      integer(kind = kint), intent(in) :: ncomp_recv, n_WR
      real(kind = kreal), intent(inout) :: WR(n_WR)
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: i, j, j0, i_field, itrans
!
!
      do j = 1, fld_rtp%num_scalar
        j0 = j + fld_rtp%istart_scalar_rtp - 1
        itrans = j+3*fld_rtp%num_vector
        do i = 1, rj_fld%num_phys
          if (fld_rtp%fld_name(j0) .eq. rj_fld%phys_name(i) ) then
            i_field = rj_fld%istack_component(i-1) + 1
            call sel_sph_rj_scalar_from_recv(iflag_recv,                &
     &          ncomp_recv, i_field, itrans, comm_rj, n_WR, WR, rj_fld)
            exit
          end if
        end do
      end do
!
      end subroutine set_all_scalar_spec_from_sph_t
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine set_all_vec_spec_to_sph_t                              &
     &         (ncomp_send, comm_rj, fld_rtp, rj_fld, n_WS, WS)
!
      use copy_spectr_4_sph_trans
!
      type(sph_comm_tbl), intent(in) :: comm_rj
      type(field_name_4_sph_trans), intent(in) :: fld_rtp
      integer(kind = kint), intent(in) :: ncomp_send, n_WS
      type(phys_data), intent(in) :: rj_fld
      real(kind = kreal), intent(inout) :: WS(n_WS)
!
      integer(kind = kint) :: i, j, j0, i_field, itrans
!
!
      do j = 1, fld_rtp%num_vector
        j0 = j + fld_rtp%istart_vector_rtp - 1
        itrans = 3*j - 2
        do i = 1, rj_fld%num_phys
          if (fld_rtp%fld_name(j0) .eq. rj_fld%phys_name(i) ) then
            i_field = rj_fld%istack_component(i-1) + 1
            call sel_sph_rj_vector_to_send                              &
     &         (ncomp_send, i_field, itrans, comm_rj, rj_fld, n_WS, WS)
            exit
          end if
        end do
      end do
!
      end subroutine set_all_vec_spec_to_sph_t
!
! -------------------------------------------------------------------
!
      subroutine set_all_vec_spec_from_sph_t(iflag_recv,                &
     &          ncomp_recv, comm_rj, fld_rtp, n_WR, WR, rj_fld)
!
      use copy_spectr_4_sph_trans
!
      integer(kind = kint), intent(in) :: iflag_recv
      type(sph_comm_tbl), intent(in) :: comm_rj
      type(field_name_4_sph_trans), intent(in) :: fld_rtp
      integer(kind = kint), intent(in) :: ncomp_recv, n_WR
      real(kind = kreal), intent(inout) :: WR(n_WR)
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: i, j, j0, i_field, itrans
!
!
      do j = 1, fld_rtp%num_vector
        j0 = j + fld_rtp%istart_vector_rtp - 1
        itrans = 3*j - 2
        do i = 1, rj_fld%num_phys
          if (fld_rtp%fld_name(j0) .eq. rj_fld%phys_name(i) ) then
            i_field = rj_fld%istack_component(i-1) + 1
            call sel_sph_rj_vector_from_recv(iflag_recv,                &
     &          ncomp_recv, i_field, itrans, comm_rj, n_WR, WR, rj_fld)
            exit
          end if
        end do
      end do
!
      end subroutine set_all_vec_spec_from_sph_t
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine set_all_tensor_spec_to_sph_t                           &
     &         (ncomp_send, comm_rj, fld_rtp, rj_fld, n_WS, WS)
!
      use copy_spectr_4_sph_trans
!
      type(sph_comm_tbl), intent(in) :: comm_rj
      type(field_name_4_sph_trans), intent(in) :: fld_rtp
      integer(kind = kint), intent(in) :: ncomp_send, n_WS
      type(phys_data), intent(in) :: rj_fld
      real(kind = kreal), intent(inout) :: WS(n_WS)
!
      integer(kind = kint) :: i, j, j0, i_field, itrans
!
!
      do j = 1, fld_rtp%num_tensor
        j0 = j + fld_rtp%istart_tensor_rtp - 1
        itrans = 1 + 6*(j-1) + fld_rtp%num_scalar                       &
     &             + 3*fld_rtp%num_vector
        do i = 1, rj_fld%num_phys
          if (fld_rtp%fld_name(j0) .eq. rj_fld%phys_name(i) ) then
            i_field = rj_fld%istack_component(i-1) + 1
            call sel_sph_rj_tensor_to_send                              &
     &         (ncomp_send, i_field, itrans, comm_rj, rj_fld, n_WS, WS)
            exit
          end if
        end do
      end do
!
      end subroutine set_all_tensor_spec_to_sph_t
!
! -------------------------------------------------------------------
!
      subroutine set_all_tensor_spec_from_sph_t(iflag_recv,             &
     &          ncomp_recv, comm_rj, fld_rtp, n_WR, WR, rj_fld)
!
      use copy_spectr_4_sph_trans
!
      integer(kind = kint), intent(in) :: iflag_recv
      type(sph_comm_tbl), intent(in) :: comm_rj
      type(field_name_4_sph_trans), intent(in) :: fld_rtp
      integer(kind = kint), intent(in) :: ncomp_recv, n_WR
      real(kind = kreal), intent(inout) :: WR(n_WR)
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: i, j, j0, i_field, itrans
!
!
      do j = 1, fld_rtp%num_tensor
        j0 = j + fld_rtp%istart_tensor_rtp - 1
        itrans = 1 + 6*(j-1) + fld_rtp%num_scalar                       &
     &             + 3*fld_rtp%num_vector
        do i = 1, rj_fld%num_phys
          if (fld_rtp%fld_name(j0) .eq. rj_fld%phys_name(i) ) then
            i_field = rj_fld%istack_component(i-1) + 1
            call sel_sph_rj_tensor_from_recv(iflag_recv,                &
     &          ncomp_recv, i_field, itrans, comm_rj, n_WR, WR, rj_fld)
            exit
          end if
        end do
      end do
!
      end subroutine set_all_tensor_spec_from_sph_t
!
!-----------------------------------------------------------------------
!
      end module copy_all_spec_4_sph_trans
