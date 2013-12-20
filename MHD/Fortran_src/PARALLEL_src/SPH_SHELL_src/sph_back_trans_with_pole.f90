!sph_back_trans_with_pole.f90
!      module sph_back_trans_with_pole
!
!        programmed by H.Matsui on Oct., 2009
!
!      subroutine sph_back_trans_vector_w_pole(is_fld, irtp_fld)
!      subroutine sph_back_trans_scalar_w_pole(is_fld, irtp_fld)
!
      module sph_back_trans_with_pole
!
      use m_precision
!
      use m_constants
      use m_addresses_trans_sph_MHD
      use copy_MHD_4_sph_trans
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sph_back_trans_vector_w_pole(is_fld, irtp_fld)
!
      use copy_spectr_4_sph_trans
      use sph_trans_vector
      use pole_sph_transform
      use copy_sph_field_4_sph_trans
!
      integer(kind = kint), intent(in)  :: is_fld, irtp_fld
!
!
      if( (is_fld*irtp_fld) .gt. 0) return
!
!$omp parallel
      call copy_vec_spec_to_trans(ithree, is_fld, ione)
!$omp end parallel
!
      call sph_b_trans_vector(ithree)
      call pole_b_trans_vector(ithree)
!
!$omp parallel
      call copy_vec_fld_from_trans(ithree, irtp_fld, ione)
!$omp end parallel
!
      end subroutine sph_back_trans_vector_w_pole
!
!-----------------------------------------------------------------------
!
      subroutine sph_back_trans_scalar_w_pole(is_fld, irtp_fld)
!
      use copy_spectr_4_sph_trans
      use sph_trans_scalar
      use pole_sph_transform
      use copy_sph_field_4_sph_trans
!
      integer(kind = kint), intent(in)  :: is_fld, irtp_fld
!
!
      if( (is_fld*irtp_fld) .gt. 0) return
!
!$omp parallel
      call copy_scalar_spec_to_trans(ione, is_fld, ione)
!$omp end parallel
!
      call sph_b_trans_scalar(ione)
      call pole_b_trans_scalar(ione)
!
!$omp parallel
      call copy_scalar_fld_from_trans(ione, irtp_fld, ione)
!$omp end parallel
!
      end subroutine sph_back_trans_scalar_w_pole
!
!-----------------------------------------------------------------------
!
      end module sph_back_trans_with_pole
