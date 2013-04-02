!add_direction_labels.f90
!
!
!      module add_direction_labels
!
!     programmed by H.Matsui on March 2012
!
!      subroutine add_vector_direction_label_xyz(field_label,           &
!     &           label_x, label_y, label_z)
!      subroutine add_vector_direction_label_rtp(field_label,           &
!     &           label_r, label_t, label_p)
!      subroutine add_vector_direction_label_cyl(field_label,           &
!     &           label_s, label_p, label_z)
!
!      subroutine add_tensor_direction_label_xyz(field_label,           &
!     &           label_xx, label_xy, label_xz, label_yy, label_yz,     &
!     &           label_zz)
!      subroutine add_tensor_direction_label_rtp(field_label,           &
!     &           label_rr, label_rt, label_rp, label_tt, label_tp,     &
!     &           label_pp)
!      subroutine add_tensor_direction_label_cyl(field_label,           &
!     &           label_ss, label_sp, label_sz, label_pp, label_pz,     &
!     &           label_zz)
!
!      subroutine add_asym_tensor_dir_label_xyz(field_label,            &
!     &           label_xy, label_xz, label_yz)
!      subroutine add_asym_tensor_dir_label_rtp(field_label,            &
!     &           label_rt, label_rp, label_tp)
!      subroutine add_asym_tensor_dir_label_cyl(field_label,            &
!     &           label_sp, label_sz, label_pz)
!
!      subroutine add_vector_sph_spectr_label(field_label,              &
!     &           label_pol, label_tor, label_tot)
!
      module add_direction_labels
!
      use m_precision
!
      implicit none
!
      character(len=kchara), parameter :: x_label = '_x'
      character(len=kchara), parameter :: y_label = '_y'
      character(len=kchara), parameter :: z_label = '_z'
      character(len=kchara), parameter :: xx_label = '_xx'
      character(len=kchara), parameter :: xy_label = '_xy'
      character(len=kchara), parameter :: xz_label = '_xz'
      character(len=kchara), parameter :: yy_label = '_yy'
      character(len=kchara), parameter :: yz_label = '_yz'
      character(len=kchara), parameter :: zz_label = '_zz'
!
      character(len=kchara), parameter :: r_label = '_r'
      character(len=kchara), parameter :: t_label = '_theta'
      character(len=kchara), parameter :: p_label = '_phi'
      character(len=kchara), parameter :: rr_label = '_rr'
      character(len=kchara), parameter :: rt_label = '_rt'
      character(len=kchara), parameter :: rp_label = '_rp'
      character(len=kchara), parameter :: tt_label = '_tt'
      character(len=kchara), parameter :: tp_label = '_tp'
      character(len=kchara), parameter :: pp_label = '_pp'
!
      character(len=kchara), parameter :: s_label = '_s'
      character(len=kchara), parameter :: ss_label = '_ss'
      character(len=kchara), parameter :: sp_label = '_sp'
      character(len=kchara), parameter :: sz_label = '_sz'
      character(len=kchara), parameter :: pz_label = '_pz'
!
      character(len=kchara), parameter :: pol_label =    '_pol'
      character(len=kchara), parameter :: dr_pol_label = '_pol_dr'
      character(len=kchara), parameter :: tor_label =    '_tor'
!
      private :: x_label, y_label, z_label
      private :: xx_label, xy_label, xz_label
      private :: yy_label, yz_label, zz_label
      private :: r_label, t_label, p_label
      private :: rr_label, rt_label, rp_label
      private :: tt_label, tp_label, pp_label
      private :: s_label, ss_label, sp_label
      private :: sz_label, pz_label
      private :: pol_label, dr_pol_label, tor_label
!
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine add_vector_direction_label_xyz(field_label,            &
     &           label_x, label_y, label_z)
!
      character(len=kchara), intent(in) :: field_label
      character(len=kchara), intent(inout) :: label_x
      character(len=kchara), intent(inout) :: label_y
      character(len=kchara), intent(inout) :: label_z
!
!
        write(label_x,'(a,a)') trim(field_label), trim(x_label)
        write(label_y,'(a,a)') trim(field_label), trim(y_label)
        write(label_z,'(a,a)') trim(field_label), trim(z_label)
!
      end subroutine add_vector_direction_label_xyz
!
!-----------------------------------------------------------------------
!
      subroutine add_vector_direction_label_rtp(field_label,            &
     &           label_r, label_t, label_p)
!
      character(len=kchara), intent(in) :: field_label
      character(len=kchara), intent(inout) :: label_r
      character(len=kchara), intent(inout) :: label_t
      character(len=kchara), intent(inout) :: label_p
!
!
        write(label_r,'(a,a)') trim(field_label), trim(r_label)
        write(label_t,'(a,a)') trim(field_label), trim(t_label)
        write(label_p,'(a,a)') trim(field_label), trim(p_label)
!
      end subroutine add_vector_direction_label_rtp
!
!-----------------------------------------------------------------------
!
      subroutine add_vector_direction_label_cyl(field_label,            &
     &           label_s, label_p, label_z)
!
      character(len=kchara), intent(in) :: field_label
      character(len=kchara), intent(inout) :: label_s
      character(len=kchara), intent(inout) :: label_p
      character(len=kchara), intent(inout) :: label_z
!
!
        write(label_s,'(a,a)') trim(field_label), trim(s_label)
        write(label_p,'(a,a)') trim(field_label), trim(p_label)
        write(label_z,'(a,a)') trim(field_label), trim(z_label)
!
      end subroutine add_vector_direction_label_cyl
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_tensor_direction_label_xyz(field_label,            &
     &           label_xx, label_xy, label_xz, label_yy, label_yz,      &
     &           label_zz)
!
      character(len=kchara), intent(in) :: field_label
      character(len=kchara), intent(inout) :: label_xx
      character(len=kchara), intent(inout) :: label_xy
      character(len=kchara), intent(inout) :: label_xz
      character(len=kchara), intent(inout) :: label_yy
      character(len=kchara), intent(inout) :: label_yz
      character(len=kchara), intent(inout) :: label_zz
!
!
        write(label_xx,'(a,a)') trim(field_label), trim(xx_label)
        write(label_xy,'(a,a)') trim(field_label), trim(xy_label)
        write(label_xz,'(a,a)') trim(field_label), trim(xz_label)
        write(label_yy,'(a,a)') trim(field_label), trim(yy_label)
        write(label_yz,'(a,a)') trim(field_label), trim(yz_label)
        write(label_zz,'(a,a)') trim(field_label), trim(zz_label)
!
      end subroutine add_tensor_direction_label_xyz
!
!-----------------------------------------------------------------------
!
      subroutine add_tensor_direction_label_rtp(field_label,            &
     &           label_rr, label_rt, label_rp, label_tt, label_tp,      &
     &           label_pp)
!
      character(len=kchara), intent(in) :: field_label
      character(len=kchara), intent(inout) :: label_rr
      character(len=kchara), intent(inout) :: label_rt
      character(len=kchara), intent(inout) :: label_rp
      character(len=kchara), intent(inout) :: label_tt
      character(len=kchara), intent(inout) :: label_tp
      character(len=kchara), intent(inout) :: label_pp
!
!
        write(label_rr,'(a,a)') trim(field_label), trim(rr_label)
        write(label_rt,'(a,a)') trim(field_label), trim(rt_label)
        write(label_rp,'(a,a)') trim(field_label), trim(rp_label)
        write(label_tt,'(a,a)') trim(field_label), trim(tt_label)
        write(label_tp,'(a,a)') trim(field_label), trim(tp_label)
        write(label_pp,'(a,a)') trim(field_label), trim(pp_label)
!
      end subroutine add_tensor_direction_label_rtp
!
!-----------------------------------------------------------------------
!
      subroutine add_tensor_direction_label_cyl(field_label,            &
     &           label_ss, label_sp, label_sz, label_pp, label_pz,      &
     &           label_zz)
!
      character(len=kchara), intent(in) :: field_label
      character(len=kchara), intent(inout) :: label_ss
      character(len=kchara), intent(inout) :: label_sp
      character(len=kchara), intent(inout) :: label_sz
      character(len=kchara), intent(inout) :: label_pp
      character(len=kchara), intent(inout) :: label_pz
      character(len=kchara), intent(inout) :: label_zz
!
!
        write(label_ss,'(a,a)') trim(field_label), trim(ss_label)
        write(label_sp,'(a,a)') trim(field_label), trim(sp_label)
        write(label_sz,'(a,a)') trim(field_label), trim(sz_label)
        write(label_pp,'(a,a)') trim(field_label), trim(pp_label)
        write(label_pz,'(a,a)') trim(field_label), trim(pz_label)
        write(label_zz,'(a,a)') trim(field_label), trim(zz_label)
!
      end subroutine add_tensor_direction_label_cyl
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_asym_tensor_dir_label_xyz(field_label,             &
     &           label_xy, label_xz, label_yz)
!
      character(len=kchara), intent(in) :: field_label
      character(len=kchara), intent(inout) :: label_xy
      character(len=kchara), intent(inout) :: label_xz
      character(len=kchara), intent(inout) :: label_yz
!
!
        write(label_xy,'(a,a)') trim(field_label), trim(xy_label)
        write(label_xz,'(a,a)') trim(field_label), trim(xz_label)
        write(label_yz,'(a,a)') trim(field_label), trim(yz_label)
!
      end subroutine add_asym_tensor_dir_label_xyz
!
!-----------------------------------------------------------------------
!
      subroutine add_asym_tensor_dir_label_rtp(field_label,             &
     &           label_rt, label_rp, label_tp)
!
      character(len=kchara), intent(in) :: field_label
      character(len=kchara), intent(inout) :: label_rt
      character(len=kchara), intent(inout) :: label_rp
      character(len=kchara), intent(inout) :: label_tp
!
!
      write(label_rt,'(a,a)') trim(field_label), trim(rt_label)
      write(label_rp,'(a,a)') trim(field_label), trim(rp_label)
      write(label_tp,'(a,a)') trim(field_label), trim(tp_label)
!
      end subroutine add_asym_tensor_dir_label_rtp
!
!-----------------------------------------------------------------------
!
      subroutine add_asym_tensor_dir_label_cyl(field_label,             &
     &           label_sp, label_sz, label_pz)
!
      character(len=kchara), intent(in) :: field_label
      character(len=kchara), intent(inout) :: label_sp
      character(len=kchara), intent(inout) :: label_sz
      character(len=kchara), intent(inout) :: label_pz
!
!
      write(label_sp,'(a,a)') trim(field_label), trim(sp_label)
      write(label_sz,'(a,a)') trim(field_label), trim(sz_label)
      write(label_pz,'(a,a)') trim(field_label), trim(pz_label)
!
      end subroutine add_asym_tensor_dir_label_cyl
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_vector_sph_spectr_label(field_label,               &
     &           label_pol, label_tor, label_dpol)
!
      character(len=kchara), intent(in) :: field_label
      character(len=kchara), intent(inout) :: label_pol
      character(len=kchara), intent(inout) :: label_tor
      character(len=kchara), intent(inout) :: label_dpol
!
!
      write(label_pol,'(a,a)') trim(field_label), trim(pol_label)
      write(label_tor,'(a,a)') trim(field_label), trim(tor_label)
      write(label_dpol,'(a,a)') trim(field_label), trim(dr_pol_label)
!
      end subroutine add_vector_sph_spectr_label
!
!-----------------------------------------------------------------------
!
      end module add_direction_labels
