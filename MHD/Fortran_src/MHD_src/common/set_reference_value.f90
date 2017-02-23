!
!     module set_reference_value
!
!      Written by H.Matsui
!      Modified by H.Matsui on Sep., 2007
!
!!      subroutine set_reference_temp(ref_param, takepito,              &
!!     &          node, fluid, i_ref, i_gref, nod_fld)
!!        type(reference_scalar_param), intent(in) :: ref_param
!!        type(takepiro_model_param), intent(in) :: takepito
!!        type(node_data), intent(in) :: node
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(phys_data), intent(inout) :: nod_fld
!
      module set_reference_value
!
      use m_precision
      use m_constants
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_reference_temp(ref_param, takepito,                &
     &          node, fluid, i_ref, i_gref, nod_fld)
!
      use t_reference_scalar_param
      use t_geometry_data
      use t_phys_data
      use t_geometry_data_MHD
!
      type(reference_scalar_param), intent(in) :: ref_param
      type(takepiro_model_param), intent(in) :: takepito
      type(node_data), intent(in) :: node
      type(field_geometry_data), intent(in) :: fluid
      integer(kind = kint), intent(in) :: i_ref, i_gref
!
      type(phys_data), intent(inout) :: nod_fld
!
!
! set reference temperature (for spherical shell)
!
      if ( ref_param%iflag_reference .eq. id_sphere_ref_temp) then
        call set_sph_shell_reftemp                                      &
     &     (ref_param%low_value, ref_param%depth_top,                   &
     &      ref_param%high_value, ref_param%depth_bottom,               &
     &      node%numnod, fluid%numnod_fld,                              &
     &      fluid%inod_fld, node%rr, node%a_r,                          &
     &      nod_fld%d_fld(1,i_ref), nod_fld%d_fld(1,i_gref))
      end if
!
      if ( ref_param%iflag_reference .eq. id_linear_r_ref_temp) then
        call set_linear_r_reftemp                                       &
     &     (ref_param%low_value, ref_param%depth_top,                   &
     &      ref_param%high_value, ref_param%depth_bottom,               &
     &      node%numnod, fluid%numnod_fld, fluid%inod_fld, node%rr,     &
     &      nod_fld%d_fld(1,i_ref), nod_fld%d_fld(1,i_gref))
      end if
!
!
      if (ref_param%iflag_reference .ge. 1                              &
     &     .and. ref_param%iflag_reference .le. 3) then
        call set_linear_reftemp(ref_param%iflag_reference,              &
     &      ref_param%low_value, ref_param%depth_top,                   &
     &      ref_param%high_value, ref_param%depth_bottom,               &
     &      node%numnod, fluid%numnod_fld, fluid%inod_fld, node%xx,     &
     &      nod_fld%d_fld(1,i_ref), nod_fld%d_fld(1,i_gref))
      end if
!
      if (ref_param%iflag_reference .eq. id_takepiro_temp) then
        call set_takepiro_temp(takepito%stratified_sigma,               &
     &     takepito%stratified_width, takepito%stratified_outer_r,      &
     &     node%numnod, fluid%numnod_fld, fluid%inod_fld, node%rr,      &
     &     nod_fld%d_fld(1,i_ref), nod_fld%d_fld(1,i_gref))
      end if
!
      end subroutine set_reference_temp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_sph_shell_reftemp                                  &
     &         (low_temp, depth_top, high_temp, depth_bottom,           &
     &          numnod, nnod_fl, inod_fluid, radius, a_radius,          &
     &          ref_remp, gref_remp)
!
      real (kind = kreal), intent(in) :: low_temp,  depth_top
      real (kind = kreal), intent(in) :: high_temp, depth_bottom
!
      integer(kind = kint), intent(in) :: numnod, nnod_fl
      real(kind = kreal), intent(in) :: radius(numnod)
      real(kind = kreal), intent(in) :: a_radius(numnod)
      integer (kind = kint), intent(in) :: inod_fluid(nnod_fl)
!
      real(kind = kreal), intent(inout) :: ref_remp(numnod)
      real(kind = kreal), intent(inout) :: gref_remp(numnod)
!
      integer (kind = kint) :: inod, inum
!
!
!$omp parallel workshare
        ref_remp(1:numnod) = zero
        gref_remp(1:numnod) = zero
!$omp end parallel workshare
!
!$omp parallel do private(inum,inod)
        do inum = 1, nnod_fl
          inod = inod_fluid(inum)
          ref_remp(inod) = ( (high_temp - low_temp)                     &
     &                        * depth_bottom*depth_top*a_radius(inod)   &
     &                        - depth_bottom*high_temp                  &
     &                        + depth_top* low_temp )                   &
     &                         / ( depth_top - depth_bottom )
          gref_remp(inod) = (-(high_temp - low_temp)                    &
     &                           * depth_bottom*depth_top)              &
     &                          / ( (depth_top-depth_bottom)            &
     &                           * radius(inod)**2 )
        end do
!$omp end parallel do
!
      end subroutine set_sph_shell_reftemp
!
! -----------------------------------------------------------------------
!
      subroutine set_linear_r_reftemp                                   &
     &         (low_temp, depth_top, high_temp, depth_bottom,           &
     &          numnod, nnod_fl, inod_fluid, radius,                    &
     &          ref_remp, gref_remp)
!
      real (kind = kreal), intent(in) :: low_temp,  depth_top
      real (kind = kreal), intent(in) :: high_temp, depth_bottom
!
      integer(kind = kint), intent(in) :: numnod, nnod_fl
      real(kind = kreal), intent(in) :: radius(numnod)
      integer (kind = kint), intent(in) :: inod_fluid(nnod_fl)
!
      real(kind = kreal), intent(inout) :: ref_remp(numnod)
      real(kind = kreal), intent(inout) :: gref_remp(numnod)
!
      integer (kind = kint) :: inod, inum
!
!
!$omp parallel workshare
        ref_remp(1:numnod) = zero
        gref_remp(1:numnod) = zero
!$omp end parallel workshare
!
!$omp parallel do private(inum,inod)
        do inum = 1, nnod_fl
          inod = inod_fluid(inum)
          ref_remp(inod) = high_temp                                    &
     &                     - (high_temp - low_temp)                     &
     &                     * (radius(inod) - depth_bottom)              &
     &                     / (depth_top - depth_bottom)
          gref_remp(inod) = -(high_temp - low_temp)                     &
     &                     / (depth_top - depth_bottom)
        end do
!$omp end parallel do
!
      end subroutine set_linear_r_reftemp
!
! -----------------------------------------------------------------------
!
      subroutine set_linear_reftemp(iflag_dir,                          &
     &          low_temp, depth_top, high_temp, depth_bottom,           &
     &          numnod, nnod_fl, inod_fluid, xx, ref_remp, gref_remp)
!
      integer(kind = kint), intent(in) :: iflag_dir
      real (kind = kreal), intent(in) :: low_temp,  depth_top
      real (kind = kreal), intent(in) :: high_temp, depth_bottom
!
      integer(kind = kint), intent(in) :: numnod, nnod_fl
      real(kind = kreal), intent(in) :: xx(numnod,3)
      integer (kind = kint), intent(in) :: inod_fluid(nnod_fl)
!
      real(kind = kreal), intent(inout) :: ref_remp(numnod)
      real(kind = kreal), intent(inout) :: gref_remp(numnod)
!
      integer (kind = kint) :: inod, inum
!
!
!$omp parallel workshare
        ref_remp(1:numnod) = zero
        gref_remp(1:numnod) = zero
!$omp end parallel workshare
!
!$omp parallel do private(inum,inod)
        do inum = 1, nnod_fl
          inod = inod_fluid(inum)
          ref_remp(inod) = (high_temp - low_temp)                       &
     &        * ( xx(inod,iflag_dir) - depth_top )                      &
     &         / ( depth_bottom - depth_top ) + low_temp
          gref_remp(inod) = (high_temp - low_temp)                      &
     &         / ( depth_bottom - depth_top )
        end do
!$omp end parallel do
!
      end subroutine set_linear_reftemp
!
! -----------------------------------------------------------------------
!
      subroutine set_takepiro_temp                                      &
     &         (stratified_sigma, stratified_width, stratified_outer_r, &
     &          numnod, nnod_fl, inod_fluid, radius,                    &
     &          ref_remp, gref_remp)
!
      real  (kind=kreal) :: stratified_sigma
      real  (kind=kreal) :: stratified_width
      real  (kind=kreal) :: stratified_outer_r
!
      integer(kind = kint), intent(in) :: numnod, nnod_fl
      real(kind = kreal), intent(in) :: radius(numnod)
      integer (kind = kint), intent(in) :: inod_fluid(nnod_fl)
!
      real(kind = kreal), intent(inout) :: ref_remp(numnod)
      real(kind = kreal), intent(inout) :: gref_remp(numnod)
!
      integer (kind = kint) :: inod, inum
      real(kind = kreal) :: alpha, beta
!
!
!$omp parallel workshare
      ref_remp(1:numnod) = zero
      gref_remp(1:numnod) = zero
!$omp end parallel workshare
!
!$omp parallel do private(inum,inod)
      do inum = 1, nnod_fl
        inod = inod_fluid(inum)
        alpha = (radius(inod)-stratified_outer_r) / stratified_width
        beta =  (radius(inod) + stratified_sigma) / stratified_width
!
        ref_remp(inod) = - half * (radius(inod) + stratified_sigma)     &
     &          * (one - tanh(alpha)) + stratified_sigma
!
        gref_remp(inod) =  half * (-one + beta)                         &
     &                     + half * tanh(alpha)                         &
     &                     - half * beta * tanh(alpha) * tanh(alpha)
      end do
!$omp end parallel do
!
      end subroutine set_takepiro_temp
!
! -----------------------------------------------------------------------
!
      end module set_reference_value

