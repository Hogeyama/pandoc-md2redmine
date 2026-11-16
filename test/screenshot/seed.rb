# Publishes every test/cases/*/golden as a wiki page of a public project.
# Run with: bin/rails runner /seed.rb
Redmine::DefaultData::Loader.load('en') if Redmine::DefaultData::Loader.no_data?
# New installs of Redmine 6 (RedMica 3) default to CommonMark.
Setting.text_formatting = 'textile'

admin = User.find_by!(login: 'admin')
project = Project.find_by(identifier: 'cases') ||
  Project.create!(name: 'cases', identifier: 'cases', is_public: true, enabled_module_names: ['wiki'])
wiki = project.wiki || Wiki.create!(project: project, start_page: 'Wiki')

Dir['/cases/*/golden'].sort.each do |golden|
  page = wiki.find_or_new_page(File.basename(File.dirname(golden)))
  content = page.content || WikiContent.new
  content.text = File.read(golden)
  content.author = admin
  page.save_with_content(content) or raise "#{golden}: #{page.errors.full_messages.join(', ')}"
end
