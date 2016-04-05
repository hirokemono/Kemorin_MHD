!
!      module t_ele_info_4_dynamic
!
!      written by Kemorin
!
!!      subroutine alloc_sgs_coefs_layer                                &
!!     &         (n_layer_d, num_sgs_kinds, num_sgs_coefs, wk_dmc)
!!      subroutine dealloc_sgs_coefs_layer(wk_dmc)
!
      module t_ele_info_4_dynamic
!
      use m_precision
      use m_constants
!
      implicit none
!
      type dynamic_model_data
!>        Number of groups for dynamic model
        integer(kind = kint) :: nlayer
!>        Number of terms for dynamic model
        integer(kind = kint) :: num_kinds
!>        Number of components for dynamic model
        integer(kind = kint) :: ntot_comp
!
        real(kind = kreal), pointer    :: fld_coef(:,:)
        real(kind = kreal), pointer    :: fld_clip(:,:)
        real(kind = kreal), pointer    :: fld_whole(:)
        real(kind = kreal), pointer    :: fld_whole_clip(:)
        real(kind = kreal), pointer    :: comp_coef(:,:)
        real(kind = kreal), pointer    :: comp_clip(:,:)
        real(kind = kreal), pointer    :: comp_whole(:)
        real(kind = kreal), pointer    :: comp_whole_clip(:)
        character(len=kchara), pointer :: name(:)
!
        real(kind = kreal), pointer    :: ave_simi(:,:)
        real(kind = kreal), pointer    :: ave_grad(:,:)
        real(kind = kreal), pointer    :: rms_simi(:,:)
        real(kind = kreal), pointer    :: rms_grad(:,:)
        real(kind = kreal), pointer    :: corrilate(:,:)
        real(kind = kreal), pointer    :: covariant(:,:)
        real(kind = kreal), pointer    :: ratio(:,:)
!
        real(kind = kreal), pointer    :: ave_simi_w(:)
        real(kind = kreal), pointer    :: ave_grad_w(:)
        real(kind = kreal), pointer    :: rms_simi_w(:)
        real(kind = kreal), pointer    :: rms_grad_w(:)
        real(kind = kreal), pointer    :: corrilate_w(:)
        real(kind = kreal), pointer    :: covariant_w(:)
        real(kind = kreal), pointer    :: ratio_w(:)
!
        real(kind = kreal), pointer    :: coef_p(:,:)
        real(kind = kreal), pointer    :: coef_wp(:)
      end type dynamic_model_data
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_sgs_coefs_layer                                  &
     &         (n_layer_d, num_sgs_kinds, num_sgs_coefs, wk_dmc)
!
      integer(kind = kint), intent(in) :: n_layer_d
      integer(kind = kint), intent(in) :: num_sgs_kinds, num_sgs_coefs
      type(dynamic_model_data), intent(inout) :: wk_dmc
!
!
      wk_dmc%nlayer = n_layer_d
      wk_dmc%num_kinds =  num_sgs_kinds
      wk_dmc%ntot_comp = num_sgs_coefs
!
       allocate(wk_dmc%comp_coef(wk_dmc%nlayer, wk_dmc%ntot_comp) )
       allocate(wk_dmc%comp_clip(wk_dmc%nlayer, wk_dmc%ntot_comp) )
       allocate(wk_dmc%comp_whole(wk_dmc%ntot_comp) )
       allocate(wk_dmc%comp_whole_clip(wk_dmc%ntot_comp) )
       allocate(wk_dmc%fld_clip(wk_dmc%nlayer, wk_dmc%num_kinds) )
       allocate(wk_dmc%fld_whole_clip(wk_dmc%num_kinds) )
       allocate(wk_dmc%fld_coef(wk_dmc%nlayer, wk_dmc%num_kinds) )
       allocate(wk_dmc%fld_whole(wk_dmc%num_kinds) )
       allocate(wk_dmc%name(wk_dmc%num_kinds) )
!
       allocate(wk_dmc%ave_simi(wk_dmc%nlayer, wk_dmc%ntot_comp) )
       allocate(wk_dmc%ave_grad(wk_dmc%nlayer, wk_dmc%ntot_comp) )
       allocate(wk_dmc%rms_simi(wk_dmc%nlayer, wk_dmc%ntot_comp) )
       allocate(wk_dmc%rms_grad(wk_dmc%nlayer, wk_dmc%ntot_comp) )
       allocate(wk_dmc%corrilate(wk_dmc%nlayer, wk_dmc%ntot_comp) )
       allocate(wk_dmc%covariant(wk_dmc%nlayer, wk_dmc%ntot_comp) )
       allocate(wk_dmc%ratio(wk_dmc%nlayer, wk_dmc%ntot_comp) )
       allocate(wk_dmc%ave_simi_w(wk_dmc%ntot_comp) )
       allocate(wk_dmc%ave_grad_w(wk_dmc%ntot_comp) )
       allocate(wk_dmc%rms_simi_w(wk_dmc%ntot_comp) )
       allocate(wk_dmc%rms_grad_w(wk_dmc%ntot_comp) )
       allocate(wk_dmc%corrilate_w(wk_dmc%ntot_comp) )
       allocate(wk_dmc%covariant_w(wk_dmc%ntot_comp) )
       allocate(wk_dmc%ratio_w(wk_dmc%ntot_comp) )
!
       allocate(wk_dmc%coef_p(wk_dmc%nlayer, wk_dmc%num_kinds) )
       allocate(wk_dmc%coef_wp(wk_dmc%num_kinds))
!
      if (wk_dmc%num_kinds .gt. 0) then
        if (wk_dmc%nlayer .gt. 0) then
          wk_dmc%fld_clip =  one
          wk_dmc%fld_coef =  one
          wk_dmc%coef_p =     one
          wk_dmc%comp_coef =  one
          wk_dmc%comp_clip =  one
        end if 
        wk_dmc%fld_whole_clip =  one
        wk_dmc%comp_whole_clip =  one
        wk_dmc%fld_whole = one
        wk_dmc%comp_whole = one
        wk_dmc%coef_wp = one
      end if
!
      if (wk_dmc%ntot_comp .gt. 0) then
        if (wk_dmc%nlayer .gt. 0) then
          wk_dmc%ave_simi =  zero
          wk_dmc%ave_grad =  zero
          wk_dmc%rms_simi =  zero
          wk_dmc%rms_grad =  zero
          wk_dmc%corrilate = zero
          wk_dmc%covariant = zero
          wk_dmc%ratio =     zero
        end if
 !
        wk_dmc%ave_simi_w =  zero
        wk_dmc%ave_grad_w =  zero
        wk_dmc%rms_simi_w =  zero
        wk_dmc%rms_grad_w =  zero
        wk_dmc%corrilate_w = zero
        wk_dmc%covariant_w = zero
        wk_dmc%ratio_w =     zero
      end if
!
      end subroutine alloc_sgs_coefs_layer
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_sgs_coefs_layer(wk_dmc)
!
      type(dynamic_model_data), intent(inout) :: wk_dmc
!
!
       deallocate(wk_dmc%fld_coef,  wk_dmc%fld_clip)
       deallocate(wk_dmc%comp_coef, wk_dmc%comp_clip)
       deallocate(wk_dmc%fld_whole, wk_dmc%comp_whole)
       deallocate(wk_dmc%fld_whole_clip)
       deallocate(wk_dmc%comp_whole_clip)
       deallocate(wk_dmc%name)
!
       deallocate(wk_dmc%ave_simi, wk_dmc%ave_grad)
       deallocate(wk_dmc%rms_simi, wk_dmc%rms_grad)
       deallocate(wk_dmc%corrilate, wk_dmc%covariant)
       deallocate(wk_dmc%ratio)
       deallocate(wk_dmc%ave_simi_w,  wk_dmc%ave_grad_w)
       deallocate(wk_dmc%rms_simi_w,  wk_dmc%rms_grad_w)
       deallocate(wk_dmc%corrilate_w, wk_dmc%covariant_w)
       deallocate(wk_dmc%ratio_w)
!
       deallocate(wk_dmc%coef_p, wk_dmc%coef_wp)
!
      end subroutine dealloc_sgs_coefs_layer
!
! ----------------------------------------------------------------------
!
      end module t_ele_info_4_dynamic
