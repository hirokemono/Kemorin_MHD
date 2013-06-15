!
!      module m_ele_info_4_dynamical
!
!      written by Kemorin
!
!      subroutine allocate_sgs_coefs_layer
!      subroutine deallocate_sgs_coefs_layer
!
      module m_ele_info_4_dynamical
!
      use m_precision
!
      implicit none
!
      real(kind = kreal), allocatable    :: sgs_f_coef(:,:)
      real(kind = kreal), allocatable    :: sgs_f_clip(:,:)
      real(kind = kreal), allocatable    :: sgs_f_whole(:)
      real(kind = kreal), allocatable    :: sgs_f_whole_clip(:)
      real(kind = kreal), allocatable    :: sgs_c_coef(:,:)
      real(kind = kreal), allocatable    :: sgs_c_clip(:,:)
      real(kind = kreal), allocatable    :: sgs_c_whole(:)
      real(kind = kreal), allocatable    :: sgs_c_whole_clip(:)
      character(len=kchara), allocatable :: name_ak_sgs(:)
!
      real(kind = kreal), allocatable    :: diff_f_coef(:,:)
      real(kind = kreal), allocatable    :: diff_f_whole(:)
      real(kind = kreal), allocatable    :: diff_f_clip(:,:)
      real(kind = kreal), allocatable    :: diff_f_whole_clip(:)
      real(kind = kreal), allocatable    :: diff_c_coef(:,:)
      real(kind = kreal), allocatable    :: diff_c_whole(:)
      real(kind = kreal), allocatable    :: diff_c_clip(:,:)
      real(kind = kreal), allocatable    :: diff_c_whole_clip(:)
      character(len=kchara), allocatable :: name_ak_diff(:)
!
      real(kind = kreal), allocatable    :: ave_sgs_simi(:,:)
      real(kind = kreal), allocatable    :: ave_sgs_grad(:,:)
      real(kind = kreal), allocatable    :: rms_sgs_simi(:,:)
      real(kind = kreal), allocatable    :: rms_sgs_grad(:,:)
      real(kind = kreal), allocatable    :: cor_sgs(:,:)
      real(kind = kreal), allocatable    :: cov_sgs(:,:)
      real(kind = kreal), allocatable    :: ratio_sgs(:,:)
!
      real(kind = kreal), allocatable    :: ave_sgs_simi_w(:)
      real(kind = kreal), allocatable    :: ave_sgs_grad_w(:)
      real(kind = kreal), allocatable    :: rms_sgs_simi_w(:)
      real(kind = kreal), allocatable    :: rms_sgs_grad_w(:)
      real(kind = kreal), allocatable    :: cor_sgs_w(:)
      real(kind = kreal), allocatable    :: cov_sgs_w(:)
      real(kind = kreal), allocatable    :: ratio_sgs_w(:)
!
      real(kind = kreal), allocatable    :: ave_diff_simi(:,:)
      real(kind = kreal), allocatable    :: ave_diff_grad(:,:)
      real(kind = kreal), allocatable    :: rms_diff_simi(:,:)
      real(kind = kreal), allocatable    :: rms_diff_grad(:,:)
      real(kind = kreal), allocatable    :: cor_diff(:,:)
      real(kind = kreal), allocatable    :: cov_diff(:,:)
      real(kind = kreal), allocatable    :: ratio_diff(:,:)
!
      real(kind = kreal), allocatable    :: ave_diff_simi_w(:)
      real(kind = kreal), allocatable    :: ave_diff_grad_w(:)
      real(kind = kreal), allocatable    :: rms_diff_simi_w(:)
      real(kind = kreal), allocatable    :: rms_diff_grad_w(:)
      real(kind = kreal), allocatable    :: cor_diff_w(:)
      real(kind = kreal), allocatable    :: cov_diff_w(:)
      real(kind = kreal), allocatable    :: ratio_diff_w(:)
!
      real(kind = kreal), allocatable    :: coef_sgs_p(:,:)
      real(kind = kreal), allocatable    :: coef_diff_p(:,:)
      real(kind = kreal), allocatable    :: coef_diff_wp(:)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_sgs_coefs_layer
!
      use m_SGS_model_coefs
      use m_layering_ele_list
!
       allocate( sgs_c_coef(n_layer_d, num_sgs_coefs) )
       allocate( sgs_c_clip(n_layer_d, num_sgs_coefs) )
       allocate( sgs_c_whole(num_sgs_coefs) )
       allocate( sgs_c_whole_clip(num_sgs_coefs) )
       allocate( sgs_f_clip(n_layer_d, num_sgs_kinds) )
       allocate( sgs_f_whole_clip(num_sgs_kinds) )
       allocate( sgs_f_coef(n_layer_d, num_sgs_kinds) )
       allocate( sgs_f_whole(num_sgs_kinds) )
       allocate( name_ak_sgs(num_sgs_kinds) )
!
       allocate( diff_c_coef(n_layer_d, num_diff_coefs) )
       allocate( diff_c_clip(n_layer_d, num_diff_coefs) )
       allocate( diff_c_whole(num_diff_coefs) )
       allocate( diff_f_coef(n_layer_d, num_diff_kinds) )
       allocate( diff_f_whole(num_diff_kinds) )
       allocate( diff_f_clip(n_layer_d, num_diff_kinds) )
       allocate( diff_f_whole_clip(num_diff_kinds) )
       allocate( diff_c_whole_clip(num_diff_coefs) )
       allocate( name_ak_diff(num_diff_coefs) )
!
       allocate( ave_sgs_simi(n_layer_d, num_sgs_coefs) )
       allocate( ave_sgs_grad(n_layer_d, num_sgs_coefs) )
       allocate( rms_sgs_simi(n_layer_d, num_sgs_coefs) )
       allocate( rms_sgs_grad(n_layer_d, num_sgs_coefs) )
       allocate( cor_sgs(n_layer_d, num_sgs_coefs) )
       allocate( cov_sgs(n_layer_d, num_sgs_coefs) )
       allocate( ratio_sgs(n_layer_d, num_sgs_coefs) )
       allocate( ave_sgs_simi_w(num_sgs_coefs) )
       allocate( ave_sgs_grad_w(num_sgs_coefs) )
       allocate( rms_sgs_simi_w(num_sgs_coefs) )
       allocate( rms_sgs_grad_w(num_sgs_coefs) )
       allocate( cor_sgs_w(num_sgs_coefs) )
       allocate( cov_sgs_w(num_sgs_coefs) )
       allocate( ratio_sgs_w(num_sgs_coefs) )
!
       allocate( ave_diff_simi(n_layer_d, num_diff_coefs) )
       allocate( ave_diff_grad(n_layer_d, num_diff_coefs) )
       allocate( rms_diff_simi(n_layer_d, num_diff_coefs) )
       allocate( rms_diff_grad(n_layer_d, num_diff_coefs) )
       allocate( cor_diff(n_layer_d, num_diff_coefs) )
       allocate( cov_diff(n_layer_d, num_diff_coefs) )
       allocate( ratio_diff(n_layer_d, num_diff_coefs) )
       allocate( ave_diff_simi_w(num_diff_coefs) )
       allocate( ave_diff_grad_w(num_diff_coefs) )
       allocate( rms_diff_simi_w(num_diff_coefs) )
       allocate( rms_diff_grad_w(num_diff_coefs) )
       allocate( cor_diff_w(num_diff_coefs) )
       allocate( cov_diff_w(num_diff_coefs) )
       allocate( ratio_diff_w(num_diff_coefs) )
!
       allocate( coef_sgs_p(n_layer_d, num_sgs_kinds) )
       allocate( coef_diff_p(n_layer_d, num_diff_kinds) )
       allocate( coef_diff_wp(num_diff_kinds) )
!
      if (num_sgs_kinds .gt. 0) then
        if (n_layer_d .gt. 0) then
          sgs_f_clip =  1.0d0
          sgs_f_coef =  1.0d0
          coef_sgs_p =  1.0d0
          sgs_c_coef =  1.0d0
          sgs_c_clip =  1.0d0
        end if 
        sgs_f_whole_clip =  1.0d0
        sgs_c_whole_clip =  1.0d0
        sgs_f_whole = 1.0d0
        sgs_c_whole = 1.0d0
      end if
!
      if (num_sgs_coefs .gt. 0) then
!
        if (n_layer_d .gt. 0) then
          ave_sgs_simi = 0.0d0
          ave_sgs_grad = 0.0d0
          rms_sgs_simi = 0.0d0
          rms_sgs_grad = 0.0d0
          cor_sgs =      0.0d0
          cov_sgs =      0.0d0
          ratio_sgs =    0.0d0
        end if
 !
        ave_sgs_simi_w = 0.0d0
        ave_sgs_grad_w = 0.0d0
        rms_sgs_simi_w = 0.0d0
        rms_sgs_grad_w = 0.0d0
        cor_sgs_w =      0.0d0
        cov_sgs_w =      0.0d0
        ratio_sgs_w =    0.0d0
      end if
!
      if (num_diff_kinds .gt. 0) then
        if (n_layer_d .gt. 0) then
          diff_f_clip =    0.0d0
          diff_f_coef =  0.0d0
          coef_diff_p =  0.0d0
        end if
        diff_f_whole_clip = 0.0d0
        diff_f_whole =      0.0d0
        diff_c_whole =      0.0d0
        coef_diff_wp =      0.0d0
        diff_c_coef =       0.0d0
        diff_c_clip =       0.0d0
        diff_c_whole_clip = 0.0d0
      end if
      if (num_diff_coefs .gt. 0) then
        if (n_layer_d .gt. 0) then
          ave_diff_simi = 0.0d0
          ave_diff_grad = 0.0d0
          rms_diff_simi = 0.0d0
          rms_diff_grad = 0.0d0
          cor_diff =      0.0d0
          cov_diff =      0.0d0
          ratio_diff =    0.0d0
        end if
!
        ave_diff_simi_w = 0.0d0
        ave_diff_grad_w = 0.0d0
        rms_diff_simi_w = 0.0d0
        rms_diff_grad_w = 0.0d0
        cor_diff_w =      0.0d0
        cov_diff_w =      0.0d0
        ratio_diff_w =    0.0d0
      end if
!
      end subroutine allocate_sgs_coefs_layer
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_sgs_coefs_layer
!
       deallocate( sgs_f_coef, sgs_f_clip, sgs_c_coef, sgs_c_clip)
       deallocate( sgs_f_whole, sgs_c_whole)
       deallocate( sgs_f_whole_clip, sgs_c_whole_clip)
       deallocate( name_ak_sgs)
       deallocate( diff_f_coef, diff_f_clip, diff_c_coef, diff_c_clip)
       deallocate( diff_f_whole, diff_c_whole)
       deallocate( diff_f_whole_clip, diff_c_whole_clip)
       deallocate( name_ak_diff)
!
       deallocate( ave_sgs_simi, ave_sgs_grad)
       deallocate( rms_sgs_simi, rms_sgs_grad)
       deallocate( cor_sgs, cov_sgs, ratio_sgs)
       deallocate( ave_sgs_simi_w, ave_sgs_grad_w)
       deallocate( rms_sgs_simi_w, rms_sgs_grad_w)
       deallocate( cor_sgs_w, cov_sgs_w, ratio_sgs_w)
!
       deallocate( ave_diff_simi, ave_diff_grad)
       deallocate( rms_diff_simi, rms_diff_grad)
       deallocate( cor_diff, cov_diff, ratio_diff, ave_diff_simi_w)
       deallocate( ave_diff_grad_w, rms_diff_simi_w, rms_diff_grad_w)
       deallocate( cor_diff_w, cov_diff_w, ratio_diff_w)
!
       deallocate( coef_sgs_p, coef_diff_p)
       deallocate( coef_diff_wp)
!
      end subroutine deallocate_sgs_coefs_layer
!
! ----------------------------------------------------------------------
!
      end module m_ele_info_4_dynamical
