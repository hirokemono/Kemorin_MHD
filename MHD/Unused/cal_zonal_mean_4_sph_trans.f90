!>@file   cal_zonal_mean_4_sph_trans.f90
!!@brief  module cal_zonal_mean_4_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2012
!
!>@brief  Copy spherical transform data to 1st FEM data
!!
!!@verbatim
!!      subroutine copy_zmean_scl_from_trans                            &
!!     &        (sph_rtp, m_folding, ncomp_trans, i_trns, d_rtp, v_pole,&
!!     &         i_field, node, nod_fld, d_zm)
!!      subroutine copy_zmean_vec_from_trans                            &
!!     &        (sph_rtp, m_folding, ncomp_trans, i_trns, d_rtp, v_pole,&
!!     &         i_field,node, nod_fld, d_zm)
!!      subroutine copy_zmean_tsr_from_trans                            &
!!     &        (sph_rtp, m_folding, ncomp_trans, i_trns, d_rtp, v_pole,&
!!     &         i_field,node, nod_fld, d_zm)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(node_data), intent(in) :: node
!!        type(phys_data),intent(inout) :: nod_fld
!!
!!      subroutine copy_zrms_scl_from_trans                             &
!!     &        (sph_rtp, m_folding, ncomp_trans, i_trns, d_rtp, v_pole,&
!!     &         i_field, node, nod_fld, d_rms)
!!      subroutine copy_zrms_vec_from_trans                             &
!!     &        (sph_rtp, m_folding, ncomp_trans, i_trns, d_rtp, v_pole,&
!!     &         i_field, node, nod_fld, d_rms)
!!      subroutine copy_zrms_tsr_from_trans                             &
!!     &        (sph_rtp, m_folding, ncomp_trans, i_trns, d_rtp, v_pole,&
!!     &         i_field, node, nod_fld, d_rms)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(node_data), intent(in) :: node
!!        type(phys_data),intent(inout) :: nod_fld
!!@endverbatim
!
      module cal_zonal_mean_4_sph_trans
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_phys_constants
!
      use t_spheric_rtp_data
      use t_geometry_data
      use t_phys_data
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_zmean_scl_from_trans                              &
     &        (sph_rtp, m_folding, ncomp_trans, i_trns, d_rtp, v_pole,  &
     &         i_field, node, nod_fld, d_zm)
!
      use copy_field_4_sph_trans
      use copy_pole_field_sph_trans
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      integer(kind = kint), intent(in) :: m_folding
      integer(kind = kint), intent(in) :: i_field, i_trns
      integer(kind = kint), intent(in) :: ncomp_trans
      real(kind = kreal), intent(in)                                    &
     &           :: d_rtp(sph_rtp%nnod_rtp,ncomp_trans)
      real(kind = kreal), intent(in)                                    &
     &           :: v_pole(sph_rtp%nnod_pole,ncomp_trans)
!
      type(node_data), intent(in) :: node
      type(phys_data),intent(inout) :: nod_fld
      real(kind = kreal), intent(inout)                                 &
     &                   :: d_zm(sph_rtp%nnod_med,n_scalar)
!
!
!$omp parallel
      call cal_sph_zonal_mean_from_trns                                 &
     &   (sph_rtp%nnod_rtp, sph_rtp%nnod_med, sph_rtp%nidx_rtp(3),      &
     &    n_scalar, d_rtp(1,i_trns), d_zm)
!$omp end parallel
!
!$omp parallel
      call copy_scalar_from_trans_smp                                   &
     &   (sph_rtp%nnod_med, sph_rtp%nidx_rtp(3),                        &
     &    node%numnod, d_zm(1,1), nod_fld%d_fld(1,i_field))
!$omp end parallel
!
      call copy_pole_scl_fld_from_trans                                 &
     &   (m_folding, sph_rtp, node, v_pole(1,i_trns), i_field, nod_fld)
!
      end subroutine copy_zmean_scl_from_trans
!
! -------------------------------------------------------------------
!
      subroutine copy_zmean_vec_from_trans                              &
     &        (sph_rtp, m_folding, ncomp_trans, i_trns, d_rtp, v_pole,  &
     &         i_field, node, nod_fld, d_zm)
!
      use copy_field_4_sph_trans
      use cvt_sph_vector_2_xyz_smp
      use copy_pole_field_sph_trans
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      integer(kind = kint), intent(in) :: m_folding
      integer(kind = kint), intent(in) :: i_field, i_trns
      integer(kind = kint), intent(in) :: ncomp_trans
      real(kind = kreal), intent(in)                                    &
     &           :: d_rtp(sph_rtp%nnod_rtp,ncomp_trans)
      real(kind = kreal), intent(in)                                    &
     &           :: v_pole(sph_rtp%nnod_pole,ncomp_trans)
!
      type(node_data), intent(in) :: node
      type(phys_data),intent(inout) :: nod_fld
      real(kind = kreal), intent(inout)                                 &
     &                   :: d_zm(sph_rtp%nnod_med,n_vector)
!
!
!$omp parallel
      call cal_sph_zonal_mean_from_trns                                 &
     &   (sph_rtp%nnod_rtp, sph_rtp%nnod_med, sph_rtp%nidx_rtp(3),      &
     &    n_vector, d_rtp(1,i_trns), d_zm)
!$omp end parallel
!
!$omp parallel
      call copy_vector_from_trans_smp                                   &
     &   (sph_rtp%nnod_med, sph_rtp%nidx_rtp(3),                        &
     &    node%numnod, d_zm(1,1), nod_fld%d_fld(1,i_field))
!$omp end parallel
!
!$omp parallel
      call overwrite_sph_vect_2_xyz_smp(np_smp, node%numnod,            &
     &    node%istack_nod_smp, nod_fld%d_fld(1,i_field),                &
     &    node%theta(1), node%phi(1))
!$omp end parallel
!
      call copy_pole_scl_fld_from_trans(m_folding, sph_rtp, node,       &
     &    v_pole(1,i_trns), i_field, nod_fld)
!
      end subroutine copy_zmean_vec_from_trans
!
! -------------------------------------------------------------------
!
      subroutine copy_zmean_tsr_from_trans                              &
     &        (sph_rtp, m_folding, ncomp_trans, i_trns, d_rtp, v_pole,  &
     &         i_field,node, nod_fld, d_zm)
!
      use copy_field_4_sph_trans
      use cvt_sph_tensor_2_xyz_smp
      use copy_pole_field_sph_trans
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      integer(kind = kint), intent(in) :: m_folding
      integer(kind = kint), intent(in) :: i_field, i_trns
      integer(kind = kint), intent(in) :: ncomp_trans
      real(kind = kreal), intent(in)                                    &
     &           :: d_rtp(sph_rtp%nnod_rtp,ncomp_trans)
      real(kind = kreal), intent(in)                                    &
     &           :: v_pole(sph_rtp%nnod_pole,ncomp_trans)
!
      type(node_data), intent(in) :: node
      type(phys_data),intent(inout) :: nod_fld
      real(kind = kreal), intent(inout)                                 &
     &                   :: d_zm(sph_rtp%nnod_med,n_sym_tensor)
!
!
!$omp parallel
      call cal_sph_zonal_mean_from_trns                                 &
     &   (sph_rtp%nnod_rtp, sph_rtp%nnod_med, sph_rtp%nidx_rtp(3),      &
     &    n_sym_tensor, d_rtp(1,i_trns), d_zm)
!$omp end parallel
!
!$omp parallel
      call copy_tensor_from_trans_smp                                   &
     &   (sph_rtp%nnod_med, sph_rtp%nidx_rtp(3),                        &
     &    node%numnod, d_zm, nod_fld%d_fld(1,i_field))
!$omp end parallel
!
!$omp parallel
      call overwrite_xyz_tensor_by_sph_smp(np_smp, node%numnod,         &
     &    node%istack_nod_smp, nod_fld%d_fld(1,i_field),                &
     &    node%xx(1,1), node%xx(1,2), node%xx(1,3),                     &
     &    node%rr(1), node%ss(1), node%a_r(1), node%a_s(1))
!$omp end parallel
!
      call copy_pole_scl_fld_from_trans(m_folding, sph_rtp, node,       &
     &    v_pole(1,i_trns), i_field, nod_fld)
!
      end subroutine copy_zmean_tsr_from_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_zrms_scl_from_trans                               &
     &        (sph_rtp, m_folding, ncomp_trans, i_trns, d_rtp, v_pole,  &
     &         i_field, node, nod_fld, d_rms)
!
      use copy_field_4_sph_trans
      use copy_pole_field_sph_trans
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      integer(kind = kint), intent(in) :: m_folding
      integer(kind = kint), intent(in) :: i_field, i_trns
      integer(kind = kint), intent(in) :: ncomp_trans
      real(kind = kreal), intent(in)                                    &
     &           :: d_rtp(sph_rtp%nnod_rtp,ncomp_trans)
      real(kind = kreal), intent(in)                                    &
     &           :: v_pole(sph_rtp%nnod_pole,ncomp_trans)
!
      type(node_data), intent(in) :: node
      type(phys_data),intent(inout) :: nod_fld
      real(kind = kreal), intent(inout)                                 &
     &                   :: d_rms(sph_rtp%nnod_med,n_scalar)
!
!
!$omp parallel
      call cal_sph_zonal_rms_from_trns                                  &
     &   (sph_rtp%nnod_rtp, sph_rtp%nnod_med, sph_rtp%nidx_rtp(3),      &
     &    n_scalar, d_rtp(1,i_trns), d_rms)
!$omp end parallel
!
!$omp parallel
      call copy_scalar_from_trans_smp                                   &
     &   (sph_rtp%nnod_med, sph_rtp%nidx_rtp(3),                        &
     &    node%numnod, d_rms(1,1), nod_fld%d_fld(1,i_field))
!$omp end parallel
!
      call copy_pole_scl_fld_from_trans                                 &
     &   (m_folding, sph_rtp, node, v_pole(1,i_trns), i_field, nod_fld)
!
      end subroutine copy_zrms_scl_from_trans
!
! -------------------------------------------------------------------
!
      subroutine copy_zrms_vec_from_trans                               &
     &        (sph_rtp, m_folding, ncomp_trans, i_trns, d_rtp, v_pole,  &
     &         i_field, node, nod_fld, d_rms)
!
      use copy_field_4_sph_trans
      use cvt_sph_vector_2_xyz_smp
      use copy_pole_field_sph_trans
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      integer(kind = kint), intent(in) :: m_folding
      integer(kind = kint), intent(in) :: i_field, i_trns
      integer(kind = kint), intent(in) :: ncomp_trans
      real(kind = kreal), intent(in)                                    &
     &           :: d_rtp(sph_rtp%nnod_rtp,ncomp_trans)
      real(kind = kreal), intent(in)                                    &
     &           :: v_pole(sph_rtp%nnod_pole,ncomp_trans)
!
      type(node_data), intent(in) :: node
      type(phys_data),intent(inout) :: nod_fld
      real(kind = kreal), intent(inout)                                 &
     &                   :: d_rms(sph_rtp%nnod_med,n_vector)
!
!
!$omp parallel
      call cal_sph_zonal_rms_from_trns                                  &
     &   (sph_rtp%nnod_rtp, sph_rtp%nnod_med, sph_rtp%nidx_rtp(3),      &
     &    n_vector, d_rtp(1,i_trns), d_rms)
!$omp end parallel
!
!$omp parallel
      call copy_vector_from_trans_smp                                   &
     &   (sph_rtp%nnod_med, sph_rtp%nidx_rtp(3),                        &
     &    node%numnod, d_rms(1,1), nod_fld%d_fld(1,i_field))
!$omp end parallel
!
!$omp parallel
      call overwrite_sph_vect_2_xyz_smp(np_smp, node%numnod,            &
     &    node%istack_nod_smp, nod_fld%d_fld(1,i_field),                &
     &    node%theta(1), node%phi(1))
!$omp end parallel
!
      call copy_pole_scl_fld_from_trans(m_folding, sph_rtp, node,       &
     &    v_pole(1,i_trns), i_field, nod_fld)
!
      end subroutine copy_zrms_vec_from_trans
!
! -------------------------------------------------------------------
!
      subroutine copy_zrms_tsr_from_trans                               &
     &        (sph_rtp, m_folding, ncomp_trans, i_trns, d_rtp, v_pole,  &
     &         i_field, node, nod_fld, d_rms)
!
      use copy_field_4_sph_trans
      use cvt_sph_tensor_2_xyz_smp
      use copy_pole_field_sph_trans
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      integer(kind = kint), intent(in) :: m_folding
      integer(kind = kint), intent(in) :: i_field, i_trns
      integer(kind = kint), intent(in) :: ncomp_trans
      real(kind = kreal), intent(in)                                    &
     &           :: d_rtp(sph_rtp%nnod_rtp,ncomp_trans)
      real(kind = kreal), intent(in)                                    &
     &           :: v_pole(sph_rtp%nnod_pole,ncomp_trans)
!
      type(node_data), intent(in) :: node
      type(phys_data),intent(inout) :: nod_fld
      real(kind = kreal), intent(inout)                                 &
     &                   :: d_rms(sph_rtp%nnod_med,n_sym_tensor)
!
!
!$omp parallel
      call cal_sph_zonal_rms_from_trns                                  &
     &   (sph_rtp%nnod_rtp, sph_rtp%nnod_med, sph_rtp%nidx_rtp(3),      &
     &    n_sym_tensor, d_rtp(1,i_trns), d_rms)
!$omp end parallel
!
!$omp parallel
      call copy_tensor_from_trans_smp                                   &
     &   (sph_rtp%nnod_med, sph_rtp%nidx_rtp(3),                        &
     &    node%numnod, d_rms, nod_fld%d_fld(1,i_field))
!$omp end parallel
!
!$omp parallel
      call overwrite_xyz_tensor_by_sph_smp(np_smp, node%numnod,         &
     &    node%istack_nod_smp, nod_fld%d_fld(1,i_field),                &
     &    node%xx(1,1), node%xx(1,2), node%xx(1,3),                     &
     &    node%rr(1), node%ss(1), node%a_r(1), node%a_s(1))
!$omp end parallel
!
      call copy_pole_scl_fld_from_trans(m_folding, sph_rtp, node,       &
     &    v_pole(1,i_trns), i_field, nod_fld)
!
      end subroutine copy_zrms_tsr_from_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_sph_zonal_mean_from_trns(nnod_rtp, nnod_med, nphi, &
     &          numdir, d_rtp, d_zm)
!
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_med, nphi
      real(kind = kreal), intent(in) :: d_rtp(nnod_rtp, numdir)
!
      real(kind = kreal), intent(inout) :: d_zm(nnod_rtp, numdir)
!
      integer(kind = kint) :: kl, m, nd, inod
!
!
!$omp workshare
      d_zm(1:nnod_med,1:numdir) = zero
!$omp end workshare
!
      do nd = 1, numdir
!$omp do private(m,kl,inod)
        do m = 1, nphi
          do kl = 1, nnod_med
            inod = kl + (m-1) * nnod_med
            d_zm(kl,nd) = d_zm(kl,nd) + d_rtp(inod,nd)
          end do
        end do
!$omp end do
      end do
!
!$omp workshare
        d_zm(1:nnod_med,1:numdir)                                       &
     &         = d_zm(1:nnod_med,1:numdir) / dble(nphi)
!$omp end workshare
!
      end subroutine cal_sph_zonal_mean_from_trns
!
!-----------------------------------------------------------------------
!
      subroutine cal_sph_zonal_rms_from_trns(nnod_rtp, nnod_med, nphi,  &
     &          numdir, d_rtp, d_rms)
!
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_med, nphi
      real(kind = kreal), intent(in) :: d_rtp(nnod_rtp, numdir)
!
      real(kind = kreal), intent(inout) :: d_rms(nnod_med, numdir)
!
      integer(kind = kint) :: kl, m, nd, inod
!
!
!$omp workshare
        d_rms(1:nnod_med,1:numdir) = zero
!$omp end workshare
!
      do nd = 1, numdir
!$omp do private(m,kl,inod)
        do m = 1, nphi
          do kl = 1, nnod_med
            inod = kl + (m-1) * nnod_med
            d_rms(kl,nd) = d_rms(kl,nd) + d_rtp(inod,nd)**2
          end do
        end do
!$omp end do
      end do
!
!$omp workshare
      d_rms(1:nnod_med,1:numdir)                                        &
     &       = sqrt(d_rms(1:nnod_med,1:numdir) / dble(nphi))
!$omp end workshare
!
      end subroutine cal_sph_zonal_rms_from_trns
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      end module cal_zonal_mean_4_sph_trans
