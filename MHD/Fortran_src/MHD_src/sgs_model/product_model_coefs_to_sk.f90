!product_model_coefs_to_sk.f90
!      module product_model_coefs_to_sk
!
!      Written by H. Matsui on Apr., 2012
!
!      subroutine prod_model_coefs_4_tensor(itype_csim, ak_sgs, sk6)
!      subroutine prod_model_coefs_4_vector(itype_csim, ak_sgs, sk6)
!      subroutine prod_model_coefs_4_asym_t(itype_csim, ak_sgs, sk6)
!
      module product_model_coefs_to_sk
!
      use m_precision
!
      use m_control_parameter
      use m_machine_parameter
      use m_geometry_parameter
      use m_geometry_data
      use m_phys_constants
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine prod_model_coefs_4_tensor(itype_csim, ak_sgs, sk6)
!
      use cvt_xyz_tensor_2_cyl_smp
      use cvt_xyz_tensor_2_sph_smp
      use cvt_cyl_tensor_2_xyz_smp
      use cvt_sph_tensor_2_xyz_smp
      use overwrite_products_smp
!
      integer (kind = kint), intent(in) :: itype_csim
      real (kind=kreal), intent(in) :: ak_sgs(1,n_sym_tensor)
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk6(numele,n_sym_tensor,nnod_4_ele)
!
      integer(kind = kint) :: k1
!
!
      if(itype_csim .eq. 1) then
        if(icoord_SGS_model_coef .eq. 1) then
!$omp parallel private(k1)
          do k1 = 1, nnod_4_ele
            call overwrite_sph_tensor_smp(np_smp, numele,               &
     &        iele_smp_stack, sk6(1,1,k1), x_ele, r_ele,                &
     &        s_ele, ar_ele, as_ele)
            call ovwrt_tensor_tensor_prod_smp(np_smp, numele,           &
     &          iele_smp_stack, ak_sgs(1,1), sk6(1,1,k1) )
            call overwrite_xyz_tensor_by_sph_smp(np_smp, numele,        &
     &        iele_smp_stack, sk6(1,1,k1), x_ele, r_ele,                &
     &        s_ele, ar_ele, as_ele)
          end do
!$omp end parallel
!
        else if(icoord_SGS_model_coef .eq. 2) then
!$omp parallel private(k1)
          do k1 = 1, nnod_4_ele
            call overwrite_cyl_tensor_smp(np_smp, numele,               &
     &        iele_smp_stack, sk6(1,1,k1), x_ele, s_ele, as_ele)
            call ovwrt_tensor_tensor_prod_smp(np_smp, numele,           &
     &          iele_smp_stack, ak_sgs(1,1), sk6(1,1,k1) )
            call overwrite_xyz_tensor_by_cyl_smp(np_smp, numele,        &
     &        iele_smp_stack, sk6(1,1,k1), x_ele, s_ele, as_ele)
          end do
!$omp end parallel
!
        else
!$omp parallel private(k1)
          do k1 = 1, nnod_4_ele
            call ovwrt_tensor_tensor_prod_smp(np_smp, numele,           &
     &          iele_smp_stack, ak_sgs(1,1), sk6(1,1,k1) )
          end do
!$omp end parallel
!
        end if
!
      else
!$omp parallel private(k1)
        do k1 = 1, nnod_4_ele
          call ovwrt_tensor_scalar_prod_smp(np_smp, numele,             &
     &        iele_smp_stack, ak_sgs(1,1), sk6(1,1,k1) )
         end do
!$omp end parallel
!
      end if
!
      end subroutine prod_model_coefs_4_tensor
!
! ----------------------------------------------------------------------
!
      subroutine prod_model_coefs_4_vector(itype_csim, ak_sgs, sk6)
!
      use cvt_xyz_vector_2_cyl_smp
      use cvt_xyz_vector_2_sph_smp
      use cvt_cyl_vector_2_xyz_smp
      use cvt_sph_vector_2_xyz_smp
      use overwrite_products_smp
!
      integer (kind = kint), intent(in) :: itype_csim
      real (kind=kreal), intent(in) :: ak_sgs(1,n_vector)
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk6(numele,n_sym_tensor,nnod_4_ele)
!
      integer(kind = kint) :: k1
!
!
      if(itype_csim .eq. 1) then
        if(icoord_SGS_model_coef .eq. 1) then
!$omp parallel private(k1)
          do k1 = 1, nnod_4_ele
            call overwrite_vector_2_sph_smp(np_smp, numele,             &
     &          iele_smp_stack, sk6(1,1,k1), x_ele, r_ele,              &
     &          s_ele, ar_ele, as_ele)
            call ovwrt_vector_vector_prod_smp(np_smp, numele,           &
     &          iele_smp_stack, ak_sgs(1,1), sk6(1,1,k1))
            call overwrite_sph_vect_2_xyz_smp(np_smp, numele,           &
     &          iele_smp_stack, sk6(1,1,k1), theta_ele, phi_ele)
          end do
!$omp end parallel
!
        else if(icoord_SGS_model_coef .eq. 2) then
!$omp parallel private(k1)
          do k1 = 1, nnod_4_ele
            call overwrite_vector_2_cyl_smp(np_smp, numele,             &
     &          iele_smp_stack, sk6(1,1,k1), x_ele, s_ele, as_ele)
            call ovwrt_vector_vector_prod_smp(np_smp, numele,           &
     &          iele_smp_stack, ak_sgs(1,1), sk6(1,1,k1))
            call overwrite_cyl_vect_2_xyz_smp(np_smp, numele,           &
     &          iele_smp_stack, sk6(1,1,k1), phi_ele)
          end do
!$omp end parallel
!
        else
!$omp parallel private(k1)
          do k1 = 1, nnod_4_ele
            call ovwrt_vector_vector_prod_smp(np_smp, numele,           &
     &          iele_smp_stack, ak_sgs(1,1), sk6(1,1,k1))
          end do
!$omp end parallel
        end if
!
      else
!$omp parallel private(k1)
        do k1 = 1, nnod_4_ele
          call ovwrt_vec_scalar_prod_smp(np_smp, numele,                &
     &        iele_smp_stack, ak_sgs(1,1), sk6(1,1,k1) )
         end do
!$omp end parallel
      end if
!
      end subroutine prod_model_coefs_4_vector
!
! ----------------------------------------------------------------------
!
      subroutine prod_model_coefs_4_asym_t(itype_csim, ak_sgs, sk6)
!
      use cvt_xyz_asym_t_2_cyl_smp
      use cvt_xyz_asym_t_2_sph_smp
      use cvt_cyl_asym_t_2_xyz_smp
      use cvt_sph_asym_t_2_xyz_smp
      use overwrite_products_smp
!
      integer (kind = kint), intent(in) :: itype_csim
      real (kind=kreal), intent(in) :: ak_sgs(1,n_sym_tensor)
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk6(numele,n_sym_tensor,nnod_4_ele)
!
      integer(kind = kint) :: k1
!
!
      if(itype_csim .eq. 1) then
        if(icoord_SGS_model_coef .eq. 1) then
!$omp parallel private(k1)
          do k1 = 1, nnod_4_ele
            call overwrite_sph_asym_t_smp(np_smp, numele,               &
     &        iele_smp_stack, sk6(1,1,k1), x_ele, r_ele,                &
     &        s_ele, ar_ele, as_ele)
            call ovwrt_tensor_tensor_prod_smp(np_smp, numele,           &
     &          iele_smp_stack, ak_sgs(1,1), sk6(1,1,k1) )
            call overwrite_xyz_asym_t_by_sph_smp(np_smp, numele,        &
     &        iele_smp_stack, sk6(1,1,k1), x_ele, r_ele,                &
     &        s_ele, ar_ele, as_ele)
          end do
!$omp end parallel
!
        else if(icoord_SGS_model_coef .eq. 2) then
!$omp parallel private(k1)
          do k1 = 1, nnod_4_ele
            call overwrite_cyl_asym_t_smp(np_smp, numele,               &
     &        iele_smp_stack, sk6(1,1,k1), x_ele, s_ele, as_ele)
            call ovwrt_tensor_tensor_prod_smp(np_smp, numele,           &
     &          iele_smp_stack, ak_sgs(1,1), sk6(1,1,k1) )
            call overwrite_xyz_asym_t_by_cyl_smp(np_smp, numele,        &
     &        iele_smp_stack, sk6(1,1,k1), x_ele, s_ele, as_ele)
          end do
!$omp end parallel
!
        else
!$omp parallel private(k1)
          do k1 = 1, nnod_4_ele
            call ovwrt_tensor_tensor_prod_smp(np_smp, numele,           &
     &          iele_smp_stack, ak_sgs(1,1), sk6(1,1,k1) )
          end do
!$omp end parallel
!
        end if
!
      else
!$omp parallel private(k1)
        do k1 = 1, nnod_4_ele
          call ovwrt_tensor_scalar_prod_smp(np_smp, numele,             &
     &        iele_smp_stack, ak_sgs(1,1), sk6(1,1,k1) )
         end do
!$omp end parallel
!
      end if
!
      end subroutine prod_model_coefs_4_asym_t
!
! ----------------------------------------------------------------------
!
      end module product_model_coefs_to_sk
