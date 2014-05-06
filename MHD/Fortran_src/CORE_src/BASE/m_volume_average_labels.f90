!m_volume_average_labels.f90
!      module m_volume_average_labels
!
!        programmed by H.Matsui on June, 2009
!
!      subroutine set_vector_label(fid_label, label_v)
!      subroutine set_sym_tensor_label(fid_label, label_st)
!
      module m_volume_average_labels
!
      use m_precision
!
      use m_phys_labels
!
      implicit none
!
!
      character(len=kchara), parameter :: e_hd_volume = 'Volume'
!
!
      character(len=kchara), parameter :: e_hd_k_ene = 'K_ene'
      character(len=kchara), parameter :: e_hd_div_v = 'div_v'
!
      character(len=kchara), parameter :: e_hd_vvec(3)                  &
     &            = (/'vx', 'vy', 'vz'/)
      character(len=kchara), parameter :: e_hd_lvec(3)                  &
     &            = (/'L_x', 'L_y', 'L_z'/)
!
!
      character(len=kchara), parameter :: e_hd_m_ene =    'M_ene'
      character(len=kchara), parameter :: e_hd_m_ene_cd = 'M_ene_cd'
      character(len=kchara), parameter :: e_hd_div_b =    'div_B'
!
      character(len=kchara), parameter :: e_hd_bvec(3)                  &
     &            = (/'Bx', 'By', 'Bz'/)
!
      character(len=kchara), parameter :: e_hd_bvec_cd(3)               &
     &            = (/'Bx_cd', 'By_cd', 'Bz_cd'/)
!
      character(len=kchara), parameter :: e_hd_div_a =    'div_A'
!
!
      character(len=kchara), parameter :: e_hd_sq_w =  'sq_w'
      character(len=kchara), parameter :: e_hd_rms_w = 'RMS_omega'
      character(len=kchara), parameter :: e_hd_wvec(3)                  &
     &            = (/'wx', 'wy', 'wz'/)
!
      character(len=kchara), parameter :: e_hd_sq_j =     'sq_J'
      character(len=kchara), parameter :: e_hd_sq_j_cd =  'sq_J_cd'
      character(len=kchara), parameter :: e_hd_rms_j =    'RMS_J'
      character(len=kchara), parameter :: e_hd_rms_j_cd = 'RMS_J_cd'
!
      character(len=kchara), parameter :: e_hd_jvec(3)                  &
     &            = (/'Jx', 'Jy', 'Jz'/)
      character(len=kchara), parameter :: e_hd_jvec_cd(3)               &
     &            = (/'Jx_cd', 'Jy_cd', 'Jz_cd'/)
!
!
      character(len=kchara), parameter                                  &
     &                      :: e_hd_fil_k_ene = 'filter_K_ene'
      character(len=kchara), parameter                                  &
     &                      :: e_hd_fil_div_v = 'div_filter_v'
!
      character(len=kchara), parameter :: e_hd_fil_vvec(3)              &
     &            = (/'filter_vx', 'filter_vy', 'filter_vz'/)
      character(len=kchara), parameter :: e_hd_fil_lvec(3)              &
     &            = (/'filter_L_x', 'filter_L_y', 'filter_L_z'/)
!
!
      character(len=kchara), parameter                                  &
     &                      :: e_hd_fil_m_ene =    'filter_M_ene'
      character(len=kchara), parameter                                  &
     &                      :: e_hd_fil_m_ene_cd = 'filter_M_ene_cd'
      character(len=kchara), parameter                                  &
     &                      :: e_hd_fil_div_b =    'div_filter_B'
!
      character(len=kchara), parameter :: e_hd_fil_bvec(3)              &
     &            = (/'filter_Bx', 'filter_By', 'filter_Bz'/)
!
      character(len=kchara), parameter :: e_hd_fil_bvec_cd(3)           &
     &            = (/'filter_Bx_cd','filter_By_cd','filter_Bz_cd'/)
!
!
      character(len=kchara), parameter                                  &
     &                      :: e_hd_fil_div_a =    'div_filter_A'
!
!
      character(len=kchara), parameter :: e_hd_induct_at(3)             &
     &            = (/'induct_t_xy', 'induct_t_xz', 'induct_t_yz'/)
!
!
      character(len=kchara), parameter :: e_hd_SGS_idct_at(3)           &
     &            = (/'SGS_induct_t_xy', 'SGS_induct_t_xz',             &
     &                'SGS_induct_t_yz'/)
!
!
      character(len=kchara), parameter :: e_hd_SGS_induct               &
     &                                   = 'SGS_induct'
!
      character(len=kchara), parameter :: e_hd_SGS_vp_induct            &
     &                                   = 'SGS_vp_induct'
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_vector_label(fid_label, label_v)
!
      use add_direction_labels
!
      character(len=kchara), intent(in) :: fid_label
      character(len=kchara), intent(inout) :: label_v(3)
!
      call add_vector_direction_label_xyz(fid_label,                    &
     &    label_v(1), label_v(2), label_v(3) )
!
      end subroutine set_vector_label
!
! ----------------------------------------------------------------------
!
      subroutine set_sym_tensor_label(fid_label, label_st)
!
      use add_direction_labels
!
      character(len=kchara), intent(in) :: fid_label
      character(len=kchara), intent(inout) :: label_st(6)
!
      call add_tensor_direction_label_xyz(fid_label,                    &
     &    label_st(1), label_st(2), label_st(3),                        &
     &    label_st(4), label_st(5), label_st(6)  )
!
      end subroutine set_sym_tensor_label
!
! ----------------------------------------------------------------------
!
      end module m_volume_average_labels
