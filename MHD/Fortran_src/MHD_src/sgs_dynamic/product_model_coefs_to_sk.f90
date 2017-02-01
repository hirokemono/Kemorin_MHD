!product_model_coefs_to_sk.f90
!      module product_model_coefs_to_sk
!
!      Written by H. Matsui on Apr., 2012
!
!!      subroutine prod_model_coefs_4_tensor(ele, itype_csim,           &
!!     &          ntot_comp_ele, icomp_sgs, ak_sgs, sk6)
!!      subroutine prod_model_coefs_4_vector(ele, itype_csim,           &
!!     &          ntot_comp_ele, icomp_sgs, ak_sgs, sk6)
!!      subroutine prod_model_coefs_4_asym_t(ele, itype_csim,           &
!!     &          ntot_comp_ele, icomp_sgs, ak_sgs, sk6)
!
      module product_model_coefs_to_sk
!
      use m_precision
!
      use m_control_parameter
      use m_machine_parameter
      use m_geometry_constants
      use m_phys_constants
!
      use t_geometry_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine prod_model_coefs_4_tensor(ele, itype_csim,             &
     &          ntot_comp_ele, icomp_sgs, ak_sgs, sk6)
!
      use cvt_xyz_tensor_2_cyl_smp
      use cvt_xyz_tensor_2_sph_smp
      use cvt_cyl_tensor_2_xyz_smp
      use cvt_sph_tensor_2_xyz_smp
      use overwrite_products_smp
!
      type(element_data), intent(in) :: ele
      integer (kind = kint), intent(in) :: itype_csim
      integer (kind = kint), intent(in) :: ntot_comp_ele, icomp_sgs
      real (kind=kreal), intent(in) :: ak_sgs(ele%numele,ntot_comp_ele)
      real (kind=kreal), intent(inout)                                  &
     &             :: sk6(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
      integer(kind = kint) :: k1
!
!
!$omp parallel private(k1)
      if(itype_csim .eq. 1) then
        if(SGS_param1%icoord_Csim .eq. iflag_spherical) then
          do k1 = 1, ele%nnod_4_ele
            call overwrite_sph_tensor_smp                               &
     &         (np_smp, ele%numele, ele%istack_ele_smp, sk6(1,1,k1),    &
     &          ele%x_ele(1:ele%numele,1),  ele%x_ele(1:ele%numele,2),  &
     &          ele%x_ele(1:ele%numele,3),                              &
     &          ele%r_ele, ele%s_ele, ele%ar_ele, ele%as_ele)
            call ovwrt_tensor_tensor_prod_smp                           &
     &         (np_smp, ele%numele, ele%istack_ele_smp,                 &
     &          ak_sgs(1,icomp_sgs), sk6(1,1,k1) )
            call overwrite_xyz_tensor_by_sph_smp                        &
     &         (np_smp, ele%numele, ele%istack_ele_smp, sk6(1,1,k1),    &
     &          ele%x_ele(1:ele%numele,1), ele%x_ele(1:ele%numele,2),   &
     &          ele%x_ele(1:ele%numele,3),                              &
     &          ele%r_ele, ele%s_ele, ele%ar_ele, ele%as_ele)
          end do
!
        else if(SGS_param1%icoord_Csim .eq. iflag_cylindrical) then
          do k1 = 1, ele%nnod_4_ele
            call overwrite_cyl_tensor_smp                               &
     &         (np_smp, ele%numele, ele%istack_ele_smp, sk6(1,1,k1),    &
     &          ele%x_ele(1:ele%numele,1), ele%x_ele(1:ele%numele,2),   &
     &          ele%s_ele, ele%as_ele)
            call ovwrt_tensor_tensor_prod_smp                           &
     &         (np_smp, ele%numele, ele%istack_ele_smp,                 &
     &          ak_sgs(1,icomp_sgs), sk6(1,1,k1) )
            call overwrite_xyz_tensor_by_cyl_smp                        &
     &         (np_smp, ele%numele, ele%istack_ele_smp, sk6(1,1,k1),    &
     &          ele%x_ele(1:ele%numele,1), ele%x_ele(1:ele%numele,2),   &
     &          ele%s_ele, ele%as_ele)
          end do
!
        else
          do k1 = 1, ele%nnod_4_ele
            call ovwrt_tensor_tensor_prod_smp(np_smp, ele%numele,       &
     &          ele%istack_ele_smp, ak_sgs(1,icomp_sgs), sk6(1,1,k1) )
          end do
        end if
!
      else
        do k1 = 1, ele%nnod_4_ele
          call ovwrt_tensor_scalar_prod_smp(np_smp, ele%numele,         &
     &        ele%istack_ele_smp, ak_sgs(1,icomp_sgs), sk6(1,1,k1) )
         end do
      end if
!$omp end parallel
!
      end subroutine prod_model_coefs_4_tensor
!
! ----------------------------------------------------------------------
!
      subroutine prod_model_coefs_4_vector(ele, itype_csim,             &
     &          ntot_comp_ele, icomp_sgs, ak_sgs, sk6)
!
      use cvt_xyz_vector_2_cyl_smp
      use cvt_xyz_vector_2_sph_smp
      use cvt_cyl_vector_2_xyz_smp
      use cvt_sph_vector_2_xyz_smp
      use overwrite_products_smp
!
      type(element_data), intent(in) :: ele
      integer (kind = kint), intent(in) :: itype_csim
      integer (kind = kint), intent(in) :: ntot_comp_ele, icomp_sgs
      real(kind=kreal), intent(in) :: ak_sgs(ele%numele,ntot_comp_ele)
      real(kind=kreal), intent(inout)                                   &
     &              :: sk6(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
      integer(kind = kint) :: k1
!
!
!$omp parallel private(k1)
      if(itype_csim .eq. 1) then
        if(SGS_param1%icoord_Csim .eq. iflag_spherical) then
          do k1 = 1, ele%nnod_4_ele
            call overwrite_vector_2_sph_smp                             &
     &         (np_smp, ele%numele, ele%istack_ele_smp, sk6(1,1,k1),    &
     &          ele%x_ele(1:ele%numele,1), ele%x_ele(1:ele%numele,2),   &
     &          ele%x_ele(1:ele%numele,3),                              &
     &          ele%r_ele, ele%s_ele, ele%ar_ele, ele%as_ele)
            call ovwrt_vector_vector_prod_smp                           &
     &         (np_smp, ele%numele, ele%istack_ele_smp,                 &
     &          ak_sgs(1,icomp_sgs), sk6(1,1,k1))
            call overwrite_sph_vect_2_xyz_smp                           &
     &         (np_smp, ele%numele, ele%istack_ele_smp,                 &
     &          sk6(1,1,k1), ele%theta_ele, ele%phi_ele)
          end do
!
        else if(SGS_param1%icoord_Csim .eq. iflag_cylindrical) then
          do k1 = 1, ele%nnod_4_ele
            call overwrite_vector_2_cyl_smp                             &
     &         (np_smp, ele%numele, ele%istack_ele_smp, sk6(1,1,k1),    &
     &          ele%x_ele(1:ele%numele,1), ele%x_ele(1:ele%numele,2),   &
     &          ele%s_ele, ele%as_ele)
            call ovwrt_vector_vector_prod_smp                           &
     &         (np_smp, ele%numele, ele%istack_ele_smp,                 &
     &          ak_sgs(1,icomp_sgs), sk6(1,1,k1))
            call overwrite_cyl_vect_2_xyz_smp                           &
     &         (np_smp, ele%numele, ele%istack_ele_smp,                 &
     &          sk6(1,1,k1), ele%phi_ele)
          end do
!
        else
          do k1 = 1, ele%nnod_4_ele
            call ovwrt_vector_vector_prod_smp(np_smp, ele%numele,       &
     &          ele%istack_ele_smp, ak_sgs(1,icomp_sgs), sk6(1,1,k1))
          end do
        end if
!
      else
        do k1 = 1, ele%nnod_4_ele
          call ovwrt_vec_scalar_prod_smp(np_smp, ele%numele,            &
     &        ele%istack_ele_smp, ak_sgs(1,icomp_sgs), sk6(1,1,k1) )
         end do
      end if
!$omp end parallel
!
      end subroutine prod_model_coefs_4_vector
!
! ----------------------------------------------------------------------
!
      subroutine prod_model_coefs_4_asym_t(ele, itype_csim,             &
     &          ntot_comp_ele, icomp_sgs, ak_sgs, sk6)
!
      use cvt_xyz_asym_t_2_cyl_smp
      use cvt_xyz_asym_t_2_sph_smp
      use cvt_cyl_asym_t_2_xyz_smp
      use cvt_sph_asym_t_2_xyz_smp
      use overwrite_products_smp
!
      type(element_data), intent(in) :: ele
      integer (kind = kint), intent(in) :: itype_csim
      integer (kind = kint), intent(in) :: ntot_comp_ele, icomp_sgs
      real(kind=kreal), intent(in) :: ak_sgs(ele%numele,ntot_comp_ele)
      real (kind=kreal), intent(inout)                                  &
     &             :: sk6(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
      integer(kind = kint) :: k1
!
!
!$omp parallel private(k1)
      if(itype_csim .eq. 1) then
        if(SGS_param1%icoord_Csim .eq. iflag_spherical) then
          do k1 = 1, ele%nnod_4_ele
            call overwrite_sph_asym_t_smp                               &
     &         (np_smp, ele%numele, ele%istack_ele_smp, sk6(1,1,k1),    &
     &          ele%x_ele(1:ele%numele,1), ele%x_ele(1:ele%numele,2),   &
     &          ele%x_ele(1:ele%numele,3),                              &
     &          ele%r_ele, ele%s_ele, ele%ar_ele, ele%as_ele)
            call ovwrt_tensor_tensor_prod_smp                           &
     &         (np_smp, ele%numele, ele%istack_ele_smp,                 &
     &          ak_sgs(1,icomp_sgs), sk6(1,1,k1) )
            call overwrite_xyz_asym_t_by_sph_smp                        &
     &         (np_smp, ele%numele, ele%istack_ele_smp, sk6(1,1,k1),    &
     &          ele%x_ele(1:ele%numele,1), ele%x_ele(1:ele%numele,2),   &
     &          ele%x_ele(1:ele%numele,3),                              &
     &          ele%r_ele, ele%s_ele, ele%ar_ele, ele%as_ele)
          end do
!
        else if(SGS_param1%icoord_Csim .eq. iflag_cylindrical) then
          do k1 = 1, ele%nnod_4_ele
            call overwrite_cyl_asym_t_smp                               &
     &         (np_smp, ele%numele, ele%istack_ele_smp, sk6(1,1,k1),    &
     &          ele%x_ele(1:ele%numele,1), ele%x_ele(1:ele%numele,2),   &
     &          ele%s_ele, ele%as_ele)
            call ovwrt_tensor_tensor_prod_smp                           &
     &         (np_smp, ele%numele, ele%istack_ele_smp,                 &
     &          ak_sgs(1,icomp_sgs), sk6(1,1,k1) )
            call overwrite_xyz_asym_t_by_cyl_smp                        &
     &         (np_smp, ele%numele, ele%istack_ele_smp, sk6(1,1,k1),    &
     &          ele%x_ele(1:ele%numele,1), ele%x_ele(1:ele%numele,2),   &
     &          ele%s_ele, ele%as_ele)
          end do
!
        else
          do k1 = 1, ele%nnod_4_ele
            call ovwrt_tensor_tensor_prod_smp(np_smp, ele%numele,       &
     &          ele%istack_ele_smp, ak_sgs(1,icomp_sgs), sk6(1,1,k1) )
          end do
!
        end if
!
      else
        do k1 = 1, ele%nnod_4_ele
          call ovwrt_tensor_scalar_prod_smp(np_smp, ele%numele,         &
     &        ele%istack_ele_smp, ak_sgs(1,icomp_sgs), sk6(1,1,k1) )
         end do
!
      end if
!$omp end parallel
!
      end subroutine prod_model_coefs_4_asym_t
!
! ----------------------------------------------------------------------
!
      end module product_model_coefs_to_sk
