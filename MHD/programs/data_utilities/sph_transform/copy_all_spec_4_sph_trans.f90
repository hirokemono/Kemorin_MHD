!copy_all_spec_4_sph_trans.f90
!      module copy_all_spec_4_sph_trans
!
!        programmed by H.Matsui on Jan., 2008
!
!!      subroutine set_all_scalar_spec_to_sph_t                         &
!!     &          (ncomp_send, n_WS, WS, v_pl_local)
!!      subroutine set_all_scalar_spec_from_sph_t(ncomp_recv, n_WR, WR)
!!
!!      subroutine set_all_vec_spec_to_sph_t(ncomp_send, n_WS, WS)
!!      subroutine set_all_vec_spec_from_sph_t(ncomp_recv, n_WR, WR)
!!
!!      subroutine set_all_tensor_spec_to_sph_t(ncomp_send, n_WS, WS)
!!      subroutine set_all_tensor_spec_from_sph_t(ncomp_recv, n_WR, WR)
!
      module copy_all_spec_4_sph_trans
!
      use m_precision
!
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_work_4_sph_trans
      use m_work_pole_sph_trans
      use set_phys_name_4_sph_trans
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
     &          (ncomp_send, n_WS, WS, v_pl_local)
!
      use copy_spectr_4_sph_trans
!
      integer(kind = kint), intent(in) :: ncomp_send, n_WS
      real(kind = kreal), intent(inout) :: WS(n_WS)
      real(kind = kreal), intent(inout)                                 &
     &                :: v_pl_local(nnod_pole,ncomp_send)
!
      integer(kind = kint) :: i, j, j0, i_field, itrans
!
!
      do j = 1, num_scalar_rtp
        j0 = j + istart_scalar_rtp - 1
        itrans = j+3*num_vector_rtp
        do i = 1, rj_fld1%num_phys
          if ( phys_name_rtp(j0) .eq. rj_fld1%phys_name(i) ) then
            i_field = rj_fld1%istack_component(i-1) + 1
            call sel_sph_rj_scalar_2_send_wpole                         &
     &         (ncomp_send, i_field, itrans, rj_fld1,                   &
     &          n_WS, WS, v_pl_local)
            exit
          end if
        end do
      end do
!
      end subroutine set_all_scalar_spec_to_sph_t
!
! -------------------------------------------------------------------
!
      subroutine set_all_scalar_spec_from_sph_t(ncomp_recv, n_WR, WR)
!
      use copy_spectr_4_sph_trans
!
      integer(kind = kint), intent(in) :: ncomp_recv, n_WR
      real(kind = kreal), intent(inout) :: WR(n_WR)
!
      integer(kind = kint) :: i, j, j0, i_field, itrans
!
!
      do j = 1, num_scalar_rtp
        j0 = j + istart_scalar_rtp - 1
        itrans = j+3*num_vector_rtp
        do i = 1, rj_fld1%num_phys
          if ( phys_name_rtp(j0) .eq. rj_fld1%phys_name(i) ) then
            i_field = rj_fld1%istack_component(i-1) + 1
            call sel_sph_rj_scalar_from_recv                            &
     &         (ncomp_recv, i_field, itrans, n_WR, WR, rj_fld1)
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
      subroutine set_all_vec_spec_to_sph_t(ncomp_send, n_WS, WS)
!
      use copy_spectr_4_sph_trans
!
      integer(kind = kint), intent(in) :: ncomp_send, n_WS
      real(kind = kreal), intent(inout) :: WS(n_WS)
!
      integer(kind = kint) :: i, j, j0, i_field, itrans
!
!
      do j = 1, num_vector_rtp
        j0 = j + istart_vector_rtp - 1
        itrans = 3*j - 2
        do i = 1, rj_fld1%num_phys
          if ( phys_name_rtp(j0) .eq. rj_fld1%phys_name(i) ) then
            i_field = rj_fld1%istack_component(i-1) + 1
            call sel_sph_rj_vector_to_send                              &
     &         (ncomp_send, i_field, itrans, rj_fld1, n_WS, WS)
            exit
          end if
        end do
      end do
!
      end subroutine set_all_vec_spec_to_sph_t
!
! -------------------------------------------------------------------
!
      subroutine set_all_vec_spec_from_sph_t(ncomp_recv, n_WR, WR)
!
      use copy_spectr_4_sph_trans
!
      integer(kind = kint), intent(in) :: ncomp_recv, n_WR
      real(kind = kreal), intent(inout) :: WR(n_WR)
!
      integer(kind = kint) :: i, j, j0, i_field, itrans
!
!
      do j = 1, num_vector_rtp
        j0 = j + istart_vector_rtp - 1
        itrans = 3*j - 2
        do i = 1, rj_fld1%num_phys
          if ( phys_name_rtp(j0) .eq. rj_fld1%phys_name(i) ) then
            i_field = rj_fld1%istack_component(i-1) + 1
!$omp parallel
            call sel_sph_rj_vector_from_recv                            &
     &         (ncomp_recv, i_field, itrans, n_WR, WR, rj_fld1)
!$omp end parallel
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
      subroutine set_all_tensor_spec_to_sph_t(ncomp_send, n_WS, WS)
!
      use copy_spectr_4_sph_trans
!
      integer(kind = kint), intent(in) :: ncomp_send, n_WS
      real(kind = kreal), intent(inout) :: WS(n_WS)
!
      integer(kind = kint) :: i, j, j0, i_field, itrans
!
!
      do j = 1, num_tensor_rtp
        j0 = j + istart_tensor_rtp - 1
        itrans = 1 + 6*(j-1) + num_scalar_rtp + 3*num_vector_rtp
        do i = 1, rj_fld1%num_phys
          if ( phys_name_rtp(j0) .eq. rj_fld1%phys_name(i) ) then
            i_field = rj_fld1%istack_component(i-1) + 1
!$omp parallel
            call sel_sph_rj_tensor_to_send                              &
     &         (ncomp_send, i_field, itrans, rj_fld1, n_WS, WS)
!$omp end parallel
            exit
          end if
        end do
      end do
!
      end subroutine set_all_tensor_spec_to_sph_t
!
! -------------------------------------------------------------------
!
      subroutine set_all_tensor_spec_from_sph_t(ncomp_recv, n_WR, WR)
!
      use m_sph_spectr_data
      use copy_spectr_4_sph_trans
!
      integer(kind = kint), intent(in) :: ncomp_recv, n_WR
      real(kind = kreal), intent(inout) :: WR(n_WR)
!
      integer(kind = kint) :: i, j, j0, i_field, itrans
!
!
      do j = 1, num_tensor_rtp
        j0 = j + istart_tensor_rtp - 1
        itrans = 1 + 6*(j-1) + num_scalar_rtp + 3*num_vector_rtp
        do i = 1, rj_fld1%num_phys
          if ( phys_name_rtp(j0) .eq. rj_fld1%phys_name(i) ) then
            i_field = rj_fld1%istack_component(i-1) + 1
!$omp parallel
            call sel_sph_rj_tensor_from_recv                            &
     &         (ncomp_recv, i_field, itrans, n_WR, WR, rj_fld1)
!$omp end parallel
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
