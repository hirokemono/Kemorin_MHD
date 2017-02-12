!cvt_dynamic_scheme_coord.f90
!      module cvt_dynamic_scheme_coord
!
!     Written by H. Matsui on Oct. 2005
!     Modified by H. Matsui on Aug., 2007
!
!> @brief Change coordinate system for dynamic model
!
!!      subroutine cvt_vector_dynamic_scheme_coord                      &
!!     &         (SGS_param, node, iphys, nod_fld)
!!      subroutine cvt_tensor_dynamic_scheme_coord                      &
!!     &         (SGS_param, node, iphys, nod_fld)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(node_data), intent(in) :: node
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(inout) :: nod_fld
!
      module cvt_dynamic_scheme_coord
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
      private :: convert_dynamic_vectors_2_sph
      private :: convert_dynamic_vectors_2_cyl
      private :: convert_dynamic_tensors_2_sph
      private :: convert_dynamic_tensors_2_cyl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cvt_vector_dynamic_scheme_coord                        &
     &         (SGS_param, node, iphys, nod_fld)
!
!
      use m_geometry_constants
      use t_SGS_control_parameter
      use t_geometry_data
      use t_phys_address
      use t_phys_data
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(inout) :: nod_fld
!
!
      if(SGS_param%icoord_Csim .eq. iflag_spherical) then
        call convert_dynamic_vectors_2_sph                              &
     &     (node%numnod, node%istack_nod_smp, node%xx,                  &
     &      node%rr, node%ss, node%a_r, node%a_s,                       &
     &      nod_fld%ntot_phys, iphys%i_sgs_simi, iphys%i_sgs_grad,      &
     &      iphys%i_sgs_grad_f, nod_fld%d_fld)
      else if(SGS_param%icoord_Csim .eq. iflag_cylindrical) then
        call convert_dynamic_vectors_2_cyl                              &
     &     (node%numnod, node%istack_nod_smp, node%xx,                  &
     &      node%ss, node%a_s, nod_fld%ntot_phys,                       &
     &      iphys%i_sgs_simi, iphys%i_sgs_grad, iphys%i_sgs_grad_f,     &
     &      nod_fld%d_fld)
      end if
!
      end subroutine cvt_vector_dynamic_scheme_coord
!
!  ---------------------------------------------------------------------
!
      subroutine cvt_tensor_dynamic_scheme_coord                        &
     &         (SGS_param, node, iphys, nod_fld)
!
      use m_machine_parameter
      use m_geometry_constants
      use t_SGS_control_parameter
      use t_geometry_data
      use t_phys_address
      use t_phys_data
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(inout) :: nod_fld
!
!
      if(SGS_param%icoord_Csim .eq. iflag_spherical) then
        call convert_dynamic_tensors_2_sph                              &
     &     (node%numnod, node%istack_nod_smp, node%xx,                  &
     &      node%rr, node%ss, node%a_r, node%a_s,                       &
     &      nod_fld%ntot_phys, iphys%i_sgs_simi, iphys%i_sgs_grad,      &
     &      iphys%i_sgs_grad_f, nod_fld%d_fld)
      else if(SGS_param%icoord_Csim .eq. iflag_cylindrical) then
      call convert_dynamic_tensors_2_cyl                                &
     &     (node%numnod, node%istack_nod_smp, node%xx,                  &
     &      node%ss, node%a_s, nod_fld%ntot_phys,                       &
     &      iphys%i_sgs_simi, iphys%i_sgs_grad, iphys%i_sgs_grad_f,     &
     &      nod_fld%d_fld)
      end if
!
      end subroutine cvt_tensor_dynamic_scheme_coord
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine convert_dynamic_vectors_2_cyl(numnod, inod_smp_stack,  &
     &          xx, s, a_s, ncomp_nod, i_sgs_simi, i_sgs_grad,          &
     &          i_sgs_grad_f, d_nod)
!
      use cvt_xyz_vector_2_cyl_smp
!
      integer (kind = kint), intent(in) :: numnod
      integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind=kreal), intent(in) :: xx(numnod,3)
      real(kind=kreal), intent(in) :: s(numnod)
      real(kind=kreal), intent(in) :: a_s(numnod)
!
      integer (kind = kint), intent(in) :: ncomp_nod, i_sgs_simi 
      integer (kind = kint), intent(in) :: i_sgs_grad, i_sgs_grad_f
      real(kind=kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
!
      if(iflag_debug.gt.0) write(*,*) 'convert cylindrical corrdinate'
!$omp parallel
      call overwrite_vector_2_cyl_smp(np_smp, numnod, inod_smp_stack,   &
     &    d_nod(1,i_sgs_simi), xx(1,1), xx(1,2), s, a_s)

      call overwrite_vector_2_cyl_smp(np_smp, numnod, inod_smp_stack,   &
     &    d_nod(1,i_sgs_grad), xx(1,1), xx(1,2), s, a_s)

      call overwrite_vector_2_cyl_smp(np_smp, numnod, inod_smp_stack,   &
     &    d_nod(1,i_sgs_grad_f), xx(1,1), xx(1,2), s, a_s)
!$omp end parallel
!
      end subroutine convert_dynamic_vectors_2_cyl
!
!  ---------------------------------------------------------------------
!
      subroutine convert_dynamic_vectors_2_sph(numnod,                  &
     &          inod_smp_stack, xx, r, s, a_r, a_s, ncomp_nod,          &
     &          i_sgs_simi, i_sgs_grad, i_sgs_grad_f, d_nod)
!
      use cvt_xyz_vector_2_sph_smp
!
      integer (kind = kint), intent(in) :: numnod
      integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind=kreal), intent(in) :: xx(numnod,3)
      real(kind=kreal), intent(in) :: r(numnod)
      real(kind=kreal), intent(in) :: s(numnod)
      real(kind=kreal), intent(in) :: a_r(numnod)
      real(kind=kreal), intent(in) :: a_s(numnod)
!
      integer (kind = kint), intent(in) :: ncomp_nod, i_sgs_simi 
      integer (kind = kint), intent(in) :: i_sgs_grad, i_sgs_grad_f
      real(kind=kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
!
      if(iflag_debug .gt. 0) write(*,*) 'convert spherical corrdinate'
!$omp parallel
      call overwrite_vector_2_sph_smp(np_smp, numnod, inod_smp_stack,   &
     &    d_nod(1,i_sgs_simi), xx(1,1), xx(1,2), xx(1,3),               &
     &    r, s, a_r, a_s)

      call overwrite_vector_2_sph_smp(np_smp, numnod, inod_smp_stack,   &
     &    d_nod(1,i_sgs_grad), xx(1,1), xx(1,2), xx(1,3),               &
     &    r, s, a_r, a_s)

      call overwrite_vector_2_sph_smp(np_smp, numnod, inod_smp_stack,   &
     &    d_nod(1,i_sgs_grad_f), xx(1,1), xx(1,2), xx(1,3),             &
     &    r, s, a_r, a_s)
!$omp end parallel
!
      end subroutine convert_dynamic_vectors_2_sph
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine convert_dynamic_tensors_2_cyl(numnod,                  &
     &          inod_smp_stack, xx, s, a_s, ncomp_nod,                  &
     &          i_sgs_simi, i_sgs_grad, i_sgs_grad_f, d_nod)
!
      use cvt_xyz_tensor_2_cyl_smp
!
      integer (kind = kint), intent(in) :: numnod
      integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind=kreal), intent(in) :: xx(numnod,3)
      real(kind=kreal), intent(in) :: s(numnod)
      real(kind=kreal), intent(in) :: a_s(numnod)
!
      integer (kind = kint), intent(in) :: ncomp_nod, i_sgs_simi 
      integer (kind = kint), intent(in) :: i_sgs_grad, i_sgs_grad_f
      real(kind=kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
!
      if(iflag_debug.gt.0) write(*,*) 'convert cylindrical corrdinate'
!$omp parallel
      call overwrite_cyl_tensor_smp(np_smp, numnod, inod_smp_stack,     &
     &    d_nod(1,i_sgs_simi), xx(1,1), xx(1,2), s, a_s)

      call overwrite_cyl_tensor_smp(np_smp, numnod, inod_smp_stack,     &
     &    d_nod(1,i_sgs_grad), xx(1,1), xx(1,2), s, a_s)

      call overwrite_cyl_tensor_smp(np_smp, numnod, inod_smp_stack,     &
     &    d_nod(1,i_sgs_grad_f), xx(1,1), xx(1,2), s, a_s)
!$omp end parallel
!
      end subroutine convert_dynamic_tensors_2_cyl
!
!  ---------------------------------------------------------------------
!
      subroutine convert_dynamic_tensors_2_sph(numnod,                  &
     &          inod_smp_stack, xx, r, s, a_r, a_s, ncomp_nod,          &
     &          i_sgs_simi, i_sgs_grad, i_sgs_grad_f, d_nod)
!
      use cvt_xyz_tensor_2_sph_smp
!
      integer (kind = kint), intent(in) :: numnod
      integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind=kreal), intent(in) :: xx(numnod,3)
      real(kind=kreal), intent(in) :: r(numnod)
      real(kind=kreal), intent(in) :: s(numnod)
      real(kind=kreal), intent(in) :: a_r(numnod)
      real(kind=kreal), intent(in) :: a_s(numnod)
!
      integer (kind = kint), intent(in) :: ncomp_nod, i_sgs_simi 
      integer (kind = kint), intent(in) :: i_sgs_grad, i_sgs_grad_f
      real(kind=kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      if(iflag_debug .gt. 0) write(*,*) 'convert spherical corrdinate'
!$omp parallel
      call overwrite_sph_tensor_smp(np_smp, numnod, inod_smp_stack,     &
     &    d_nod(1,i_sgs_simi), xx(1,1), xx(1,2), xx(1,3),               &
     &    r, s, a_r, a_s)
!
      call overwrite_sph_tensor_smp(np_smp, numnod, inod_smp_stack,     &
     &    d_nod(1,i_sgs_grad), xx(1,1), xx(1,2), xx(1,3),               &
     &    r, s, a_r, a_s)
!
      call overwrite_sph_tensor_smp(np_smp, numnod, inod_smp_stack,     &
     &    d_nod(1,i_sgs_grad_f), xx(1,1), xx(1,2), xx(1,3),             &
     &    r, s, a_r, a_s)
!$omp end parallel
!
      end subroutine convert_dynamic_tensors_2_sph
!
!  ---------------------------------------------------------------------
!
      end module cvt_dynamic_scheme_coord
