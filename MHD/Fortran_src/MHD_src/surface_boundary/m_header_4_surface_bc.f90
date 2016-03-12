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
      character(len=kchara), parameter :: name_sv(3)                    &
     &     = (/'velocity_x_surf','velocity_y_surf','velocity_z_surf'/)
      character(len=kchara), parameter :: name_svn = 'normal_velocity'
!
      character(len=kchara), parameter :: name_sa(3)                    &
     &     = (/'vector_potential_x_surf','vector_potential_y_surf',     &
     &         'vector_potential_z_surf'/)
      character(len=kchara), parameter                                  &
     &      :: name_san = 'normal_vector_potential'
!
      character(len=kchara), parameter :: name_sb(3)                    &
     &     = (/'magnetic_field_x_surf','magnetic_field_y_surf',         &
     &         'magnetic_field_z_surf'/)
      character(len=kchara), parameter                                  &
     &      :: name_sbn = 'normal_magnetic_field'
!
      character(len=kchara), parameter :: name_sj(3)                    &
     &     = (/'current_x_surf','current_y_surf','current_z_surf'/)
      character(len=kchara), parameter :: name_sjn = 'normal_current'
!
!
      character(len=kchara), parameter                                  &
     &      :: name_smp = 'magnetic_potential_surf'
!
!
!
      character(len=kchara), parameter :: name_vg(3)                    &
     &     = (/'grad_velocity_x','grad_velocity_y','grad_velocity_z'/)
      character(len=kchara), parameter                                  &
     &      :: name_vng = 'grad_normal_velocity'
!
      character(len=kchara), parameter :: name_ag(3)                    &
     &     = (/'grad_vector_potential_x','grad_vector_potential_y',     &
     &         'grad_vector_potential_z'/)
      character(len=kchara), parameter                                  &
     &      :: name_ang = 'grad_normal_vector_potential'
!
      character(len=kchara), parameter :: name_bg(3)                    &
     &       = (/'grad_magnetic_field_x','grad_magnetic_field_y',       &
     &           'grad_magnetic_field_z'/)
      character(len=kchara), parameter                                  &
     &      :: name_bng = 'grad_normal_magnetic_field'
!
      character(len=kchara), parameter :: name_jg(3)                    &
     &       = (/'grad_current_x','grad_current_y','grad_current_z'/)
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
