!
!     module m_header_4_surface_bc
!
!      Written by H. Matsui on Sep. 2005
!
      module m_header_4_surface_bc
!
      use m_precision
!
      implicit  none
!
      character(len=kchara), parameter :: name_svx = 'velocity_x_surf'
      character(len=kchara), parameter :: name_svy = 'velocity_y_surf'
      character(len=kchara), parameter :: name_svz = 'velocity_z_surf'
      character(len=kchara), parameter :: name_svn = 'normal_velocity'
!
      character(len=kchara), parameter                                  &
     &      :: name_sax = 'vector_potential_x_surf'
      character(len=kchara), parameter                                  &
     &      :: name_say = 'vector_potential_y_surf'
      character(len=kchara), parameter                                  &
     &      :: name_saz = 'vector_potential_z_surf'
      character(len=kchara), parameter                                  &
     &      :: name_san = 'normal_vector_potential'
!
      character(len=kchara), parameter                                  &
     &      :: name_sbx = 'magnetic_field_x_surf'
      character(len=kchara), parameter                                  &
     &      :: name_sby = 'magnetic_field_y_surf'
      character(len=kchara), parameter                                  &
     &      :: name_sbz = 'magnetic_field_z_surf'
      character(len=kchara), parameter                                  &
     &      :: name_sbn = 'normal_magnetic_field'
!
      character(len=kchara), parameter :: name_sjx = 'current_x_surf'
      character(len=kchara), parameter :: name_sjy = 'current_y_surf'
      character(len=kchara), parameter :: name_sjz = 'current_z_surf'
      character(len=kchara), parameter :: name_sjn = 'normal_current'
!
!
      character(len=kchara), parameter :: name_st = 'temperature_surf'
      character(len=kchara), parameter :: name_sp = 'pressure_surf'
      character(len=kchara), parameter                                  &
     &      :: name_smp = 'magnetic_potential_surf'
      character(len=kchara), parameter :: name_sds = 'composition_surf'
!
!
!
      character(len=kchara), parameter :: name_vxg = 'grad_velocity_x'
      character(len=kchara), parameter :: name_vyg = 'grad_velocity_y'
      character(len=kchara), parameter :: name_vzg = 'grad_velocity_z'
      character(len=kchara), parameter                                  &
     &      :: name_vng = 'grad_normal_velocity'
!
      character(len=kchara), parameter                                  &
     &      :: name_axg = 'grad_vector_potential_x'
      character(len=kchara), parameter                                  &
     &      :: name_ayg = 'grad_vector_potential_y'
      character(len=kchara), parameter                                  &
     &      :: name_azg = 'grad_vector_potential_z'
      character(len=kchara), parameter                                  &
     &      :: name_ang = 'grad_normal_vector_potential'
!
      character(len=kchara), parameter                                  &
     &      :: name_bxg = 'grad_magnetic_field_x'
      character(len=kchara), parameter                                  &
     &      :: name_byg = 'grad_magnetic_field_y'
      character(len=kchara), parameter                                  &
     &      :: name_bzg = 'grad_magnetic_field_z'
      character(len=kchara), parameter                                  &
     &      :: name_bng = 'grad_normal_magnetic_field'
!
      character(len=kchara), parameter :: name_jxg = 'grad_current_x'
      character(len=kchara), parameter :: name_jyg = 'grad_current_y'
      character(len=kchara), parameter :: name_jzg = 'grad_current_z'
      character(len=kchara), parameter                                  &
     &      :: name_jng = 'grad_normal_current'
!
!
      character(len=kchara), parameter :: name_hf = 'heat_flux'
      character(len=kchara), parameter :: name_pg = 'pressure_grad'
      character(len=kchara), parameter                                  &
     &      :: name_mpg = 'magnetic_potential_grad'
      character(len=kchara), parameter :: name_dsg = 'dummy_scalar_grad'
!
      end module m_header_4_surface_bc
