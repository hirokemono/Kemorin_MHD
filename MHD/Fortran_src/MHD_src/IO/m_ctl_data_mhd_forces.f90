!m_ctl_data_mhd_forces.f90
!      module m_ctl_data_mhd_forces
!
!        programmed by H.Matsui on March. 2006
!
!      subroutine read_forces_control
!      subroutine read_gravity_control
!      subroutine read_coriolis_control
!      subroutine read_magneto_control
!
!    begin forces_define
!!!!!  define of forces !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  available forces
!     gravity, Coriolis, Lorentz, Composite_gravity
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      array force_ctl      4
!!        force_ctl   gravity              end
!!        force_ctl   Coriolis             end
!!        force_ctl   Lorentz              end
!!        force_ctl   Composite_gravity    end
!!      end array
!!    end  forces_define
!!
!! !!!! gravity_type !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      0: constant
!!      1: constant_radial (constant intensity)
!!      2: radial (propotional to radius)
!! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    begin gravity_define
!!      gravity_type_ctl     radial
!!
!! !!!! direction of gravity (opposite direction to that of buoyancy)
!!      array gravity_vec  3
!!        gravity_vec  x     0.000   end
!!        gravity_vec  y     0.000   end
!!        gravity_vec  z     -1.000   end
!!      end array gravity_vec
!!    end  gravity_define
!!
!! !!!! direction of rotation vector for Coriolis force !!!!!!!!!!!!!
!!
!!    begin Coriolis_define
!!      array rotation_vec   3
!!        rotation_vec  x   0.000    end
!!        rotation_vec  y   0.000    end
!!        rotation_vec  z   1.000    end
!!      end array rotation_vec
!!
!!      tri_sph_int_file     'rot_int.dat'
!!      sph_int_file_format     'ascii'
!!    end  Coriolis_define
!!
!!!!!!!!!!  magnetoconvection model!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    array ext_magne_vec:   0...off  more than 1...On
!!     ext_magne_vec: external field (constant)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    begin Magneto_convection_def
!!      magneto_cv_ctl    On
!!      array ext_magne_vec   3
!!        ext_magne_vec  x     0.000   end
!!        ext_magne_vec  y     1.000   end
!!        ext_magne_vec  z     0.000   end
!!      end array ext_magne_vec
!!    end  Magneto_convection_def
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
      module m_ctl_data_mhd_forces
!
      use m_precision
!
      use t_ctl_data_mhd_forces
!
      implicit  none
!
!>      Structure for force list
      type(forces_control), save :: frc_ctl1
!>      Structure for gravity definistion
      type(gravity_control), save :: g_ctl1
!>      Structure for Coriolis force
      type(coriolis_control), save :: cor_ctl1
!>      Structure for Coriolis force
      type(magneto_convection_control), save :: mcv_ctl1
!
!   entry label
!
      character(len=kchara), parameter                                  &
     &      :: hd_forces_ctl =   'forces_define'
      integer (kind=kint) :: i_forces_ctl =    0
!
      character(len=kchara), parameter                                  &
     &      :: hd_gravity_ctl =  'gravity_define'
      integer (kind=kint) :: i_gravity_ctl =   0
!
      character(len=kchara), parameter                                  &
     &      :: hd_coriolis_ctl = 'Coriolis_define'
      integer (kind=kint) :: i_coriolis_ctl =  0
!
      character(len=kchara), parameter                                  &
     &      :: hd_magneto_ctl =  'Magneto_convection_def'
      integer (kind=kint) :: i_magneto_ctl =   0
!
!
      private :: hd_forces_ctl, i_forces_ctl
      private :: hd_gravity_ctl, hd_coriolis_ctl, hd_magneto_ctl
      private :: i_gravity_ctl,  i_coriolis_ctl,  i_magneto_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_forces_control
!
!
      call read_forces_ctl(hd_forces_ctl, i_forces_ctl, frc_ctl1)
!
      end subroutine read_forces_control
!
!   --------------------------------------------------------------------
!
      subroutine read_gravity_control
!
!
      call read_gravity_ctl(hd_gravity_ctl, i_gravity_ctl, g_ctl1)
!
      end subroutine read_gravity_control
!
!   --------------------------------------------------------------------
!
      subroutine read_coriolis_control
!
!
      call read_coriolis_ctl(hd_coriolis_ctl, i_coriolis_ctl, cor_ctl1)
!
      end subroutine read_coriolis_control
!
!   --------------------------------------------------------------------
!
      subroutine read_magneto_control
!
!
      call read_magneto_ctl(hd_magneto_ctl, i_magneto_ctl, mcv_ctl1)
!
      end subroutine read_magneto_control
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_mhd_forces
